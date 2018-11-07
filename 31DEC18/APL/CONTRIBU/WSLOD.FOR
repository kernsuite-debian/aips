LOCAL INCLUDE 'WSLOD.INC'
C                                                          Include DWIN
      INTEGER   MAXCH
      PARAMETER (MAXCH = 128)
C
      DOUBLE PRECISION BSC, BZE, RA1, DEC1, RA0, DEC0, BASLM(14),
     *          BFREQ(MAXCH)
      HOLLERITH XNAMEI(3), XNAMEO(3), XCLAOU(2), XUVWTF
      CHARACTER NAMEIN*12, NAMOUT*12, CLAOUT*6
      REAL      TAPE, XFILES,
     *          DISO, XVSMAX, PSCAL(91,4,MAXCH), BASNO(91),
     *          XBBAND, XEBAND, XIBAND,
     *          BASLIN(91), SHA(91), DHA(91), BANDW, WVSMAX,
     *          DAYFR, PHIROT, NAMPAK(3), CLSPAK(2), STAT, XBAND
      REAL      BUFF1(4610),BUFF2(4610)
      CHARACTER*6 CLAOUT1, CLAOUT2
      CHARACTER*2 UVWTFN
      REAL      DONOTST
      INTEGER   NVIS
      INTEGER   NVISFL(2)
      LOGICAL   NDOEOF, ISBLNK, LCMPLX, LSTOKE, RQUICK, DOLIST
      INTEGER   LUN1, LUN2, IND1, IND2, DISOUT1, DISOUT2, CCNO1, CCNO2
      INTEGER   BUFF0(256), HW(3840), CW(1920)
      INTEGER   DOGBLK1(20), DOGBLK2(20)
      INTEGER   LREC, NFILES, DISOUT, TAPEIN, GROUP,
     *          ICEND, IBLANK(2), TABLES, IBPP, NPARMS, ITAB(20),
     *          JADR(4), NSTACT, STRTRC(91,4,MAXCH),
     *          IINT(91), LUNDS, DINDS,
     *          NDATP(91), POFF(91,4,MAXCH), NRINT,
     *          WDATE(6), CCNO, POLC,
     *          ICNO, SNO, WS, NRPOL, NSTOKES, IVOL, IVER, BBAND,
     *          EBAND, IBAND, FDVEC(50)
      COMMON /BUFRS / BUFF2, BUFF1, BUFF0, HW, CW
      COMMON /INPARM/ XNAMEI, XBBAND, XEBAND, XIBAND,
     *                TAPE, XFILES, XNAMEO,
     *                XCLAOU, DISO, XUVWTF, DONOTST, XVSMAX
      COMMON /SCRINF/ NVIS, NDOEOF, RQUICK, DOLIST,
     *                NFILES, DISOUT, TAPEIN,
     *                LREC, LUNDS, DINDS,
     *                BBAND, EBAND, IBAND, FDVEC
      COMMON /FITINF/ BSC, BZE, ISBLNK, LCMPLX, LSTOKE, ICEND,
     *                IBLANK, TABLES, IBPP, NPARMS, ITAB, JADR, NSTACT
      COMMON /SRTDAT/ RA0, DEC0, RA1, DEC1, BASLM,  PSCAL, BASNO,
     *                BASLIN, SHA, DHA, BFREQ, BANDW, WVSMAX, DAYFR,
     *                NDATP, POFF, PHIROT, STRTRC, IINT, NRINT, WDATE,
     *                CCNO, POLC, NRPOL, NSTOKES
      COMMON /FILES/  NAMPAK, CLSPAK, STAT, ICNO, SNO, WS, IVOL, IVER
      COMMON /FINFO/  LUN1, LUN2, IND1, IND2,
     *                DISOUT1, DISOUT2, CCNO1, CCNO2, NVISFL,
     *                DOGBLK1, DOGBLK2
      COMMON /CHACOM/ NAMEIN, NAMOUT, CLAOUT, CLAOUT1, CLAOUT2
LOCAL END
      PROGRAM WSLOD
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C----------------------------------------------------------------------
C   WSLOD copies a uv data base from a WSRT DR tape to a catalogued
C   disk file.  If the input name is set to 'INDEX' it will read
C   the entire input tape, and write out relevant information on
C   each file.
C  Inputs:
C  Adverb  Pgm. name         Comments.
C    NAMEIN    NAMEIN     NAMEIN name; if = 'INDEX' index the tape
C    BCHAN     BBAND      1st WSRT band number (0 => continuum)
C    ECHAN     EBAND      last WSRT band number
C    CHINC     IBAND      delta WSRT band number
C              sequence number of output file := channel number
C
C    INTAPE    TAPEIN     Input tape drive number
C    NFILES    NFILES     Label on tape to read in
C                         < 0 => quit when one full sour/vis set read
C    OUTNAME   NAMOUT     Output file name (default = NAMEIN)
C    OUTCLASS  CLAOUT     Output file class (default = 'UVDATA' if
C                                           donotst <> 3, else 'UVRDUN')
C    OUTDISK   DISOUT     Output disk volume
C    UVWTFN    UVWTFN     Reweight WSRT data to near uniform
C    DOALL     DONOTST    If 1 include the non standard WSRT baselines,
C                         if 2 save original IF pairs,
C                         if 3 write redundant and non-redundant data
C                              to different files. Note that the meaning
C                              of 'redundant' is slightly different from
C                              the meaning of 'standard'.
C
C
C
C  Programmer WaWa, Oct. 1982
C  Modified April 1984 by R.J.Hanisch to handle WSRT tape format 6.
C  Latest modifications to handle redundant baselines by:
C    J. M. van der Hulst; September 1985, Dwingeloo, the Netherlands.
C    Bert van Oortmarssen; Augustus 1987, Leiden, the Netherlands.
C         < DUMPAN corrected (to new AN-file format) >
C         < check for duplicate baselines corrected  >
C         < ignore IH-blocks 92..96 (dummy blocks)   > 27-04-88
C         < range of band numbers                    > 19-05-88
C  Mark Wieringa; Nov. 1988, Leiden, The Netherlands :
C  added system 52 format : integration times now calculated from HA-step.
C  WaWa again March 1989 to integrate charlottesville tapeio with
C  Leiden/Dwingeloo changes in format, and rationalize multiple channel
C  numbers.
C  WaWa   Feb. 1990, conform to new NRAO code revisions
C----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER IERR, IRET
      INTEGER   FTAB(160), INDEST
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:IDCH.INC'
      DATA PRGM /'WSLOD '/
C----------------------------------------------------------------------
C                                       Get input parameters
      CALL UVPARM (PRGM, IERR)
      IRET = 16
      IF (IERR.NE.0) GO TO 990
C                                       Read vis tape, copy to
C                                       catalogued file.
         CALL VISIN (IERR)
C                                       Need to destroy output file
         IF (IERR.GT.10) GO TO 950
C                                       Leave output alone
         IF (IERR.NE.0)  GO TO 990
C                                       Finished
         IRET = 0
         GO TO 990
  950  CONTINUE
C                                   Clear status, then destroy
      CALL FILEINFO(1)
      CALL PTBLK (1, 'WRIT')
      CALL CATIO ('READ', DISOUT, CCNO, CATBLK, 'CLWR', BUFF0, IERR)
      CALL MDESTR (DISOUT, CCNO, CATBLK, BUFF0, INDEST, IERR)
      IF (DONOTST.EQ.3.0) THEN
C                        Clear and destroy 2nd file
         CALL FILEINFO(2)
         CALL PTBLK (2, 'WRIT')
         CALL CATIO ('READ', DISOUT, CCNO, CATBLK, 'CLWR', BUFF0, IERR)
         CALL MDESTR (DISOUT, CCNO, CATBLK, BUFF0, INDEST, IERR)
      END IF
C
  990 CONTINUE
      IF (.NOT.DOLIST) THEN
         CALL FILEINFO(1)
C                        not necessary, but does not harm
         CALL PTBLK (1, 'WRIT')
         CALL UNSCR
      ENDIF
C
      CALL DIETSK (IRET, RQUICK, BUFF0)
C
      STOP
      END
C
C
C
      SUBROUTINE UVPARM (PRGM, JERR)
C----------------------------------------------------------------------
C   UVPARM reads input parameters for Task UVLOD.  See header comments
C   in PROGRAM UVLOD for more details.
C   Inputs: PRGM   C*6      Program name
C           IERR   I        Error code: 0 => ok, else quit
C   Common: /MAPHDR/ initial cat block header (heavily filled in if
C                    FITS is false)
C   Programmer: W. D. Cotton, Jan. 1981.
C----------------------------------------------------------------------
      CHARACTER*(*) PRGM
      INTEGER JERR
      LOGICAL   EQUAL
      INTEGER IRET, IERR
      INTEGER   NAXIS, NRAN, INC, IFIL, I, INDEX
      CHARACTER*8 IRAN(5), ICOOR(5)
      REAL   TEMP(2), EPS
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C                               I*4 Buffer Size
      INTEGER  FDVEC3_4
      EQUIVALENCE (FDVEC3_4, FDVEC(3))
C
      DATA IRAN /'UU-L    ','VV-L    ','WW-L    ',
     *           'BASELINE','TIME1   '/
      DATA ICOOR /'COMPLEX ','STOKES  ','FREQ    ',
     *           'RA---NCP','DEC--NCP'/
      DATA NAXIS, NRAN /5, 5/
C----------------------------------------------------------------------
C                                       Initilize I/O
      CALL ZDCHIN (.TRUE.)
      CALL HIINIT (3)
      CALL VHDRIN
      JERR = 0
C                                       Get input parameters.
      NPARMS = 16
      CALL GTPARM (PRGM, NPARMS, RQUICK, XNAMEI, BUFF0, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         GO TO 20
 10   CALL FILL (256, 0, CATBLK)
      CALL FILL (50,  0, FDVEC)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (12, 1, XNAMEO, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (2, 1, XUVWTF, UVWTFN)
      DOLIST = NAMEIN(1:5) .EQ. 'INDEX'
      EPS = 0.1
C                                       Get TAPEIN
      TAPEIN = TAPE + EPS
C                                       Get NFILES
C            Account for header and trailer labels
      NFILES = 3.0 * XFILES - 2.0 + EPS
      IF (DOLIST) THEN
         MSGTXT = 'I will try to list an entire WSRT tape'
         CALL MSGWRT(2)
      ENDIF
C                                       Get DISOUT( ), DISOUT is used
C                                       only when DONOTST=3
      DISOUT  = DISO + EPS
      DISOUT1 = DISOUT
      DISOUT2 = DISOUT
C                                       Get band info.
      BBAND = XBBAND + EPS
      EBAND = XEBAND + EPS
      IBAND = XIBAND + EPS
C                                       rationalize
      BBAND = MAX (BBAND, 0)
      IBAND = MAX (IBAND, 1)
      EBAND = MAX (EBAND, BBAND)
      EBAND = MIN (EBAND, BBAND + IBAND * (MAXCH-1))
C                                       Get max. no. visibilities.
C                                       Set user id in CATBLK
      CATBLK(KIIMU) = NLUSER
C                                       If RQUICK, restart AIPS
      IRET = 0
C                                       Test legality
      IF (NTAPED.EQ.1 .OR. TAPEIN.LT.1) TAPEIN = 1
      IF ((TAPEIN.GE.1) .AND. (TAPEIN.LE.NTAPED)) GO TO 15
         WRITE (MSGTXT, 1010) TAPEIN
         CALL MSGWRT (8)
         IRET = 8
         GO TO 20
 15   IF ((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) GO TO 20
         WRITE (MSGTXT, 1015)
         CALL MSGWRT (8)
         IRET = 8
 20   JERR = IRET
      IF (RQUICK) CALL RELPOP (IRET, BUFF0, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Open tape
      FDVEC(1)  = TAPEIN + 30
      FDVEC(2)  = 3840
      FDVEC3_4  = 7680
      FDVEC(5)  = TAPEIN
      FDVEC(31) = 0
C
      CALL TAPIO ('OPRD', FDVEC(1), HW, FDVEC(40), IERR)
      IF (IERR.LE.1) GO TO 25
         WRITE (MSGTXT, 1020) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
C                                       Skip to correct file.
 25   IF (NFILES.GT.0) THEN
           WRITE (MSGTXT, 1022) NFILES
           CALL MSGWRT(2)
           CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IERR)
      ENDIF
      IFIL = 1-NFILES
      IF (NFILES.LE.0)
     *          CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT, 1025) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
 30   MSGSUP = 32000
C                                        enable level 6 & 7 messages.
         MSGSUP = 0
      IF (DOLIST) GO TO 999
C
C                                       Get CLAOUT in CATBLK
         IF (DONOTST.NE.3.0) THEN
C                                       Check if not 'UVNORD', 'UVRDUN'
            IF ((CLAOUT .EQ. 'UVNORD') .OR. (CLAOUT .EQ. 'UVRDUN'))
     *          THEN
               WRITE (MSGTXT, 1012) CLAOUT1, DONOTST
               CALL MSGWRT (6)
               CLAOUT1 = 'UVDATA'
               WRITE (MSGTXT, 1013) CLAOUT1
               CALL MSGWRT (6)
            ELSE IF (CLAOUT .EQ. ' ') THEN
               CLAOUT1 = 'UVDATA'
            ENDIF
            CALL CHR2H (6, CLAOUT1, KHIMCO, CATH(KHIMC))
         ELSE
            CLAOUT1 = 'UVRDUN'
            CLAOUT2 = 'UVNORD'
            CALL CHR2H (6, CLAOUT1, KHIMCO, CATH(KHIMC))
C                                       CAT4(K4IMC) has to be filled
C                                       with CLAOUT2 after the first
C                                       file is created.
         END IF
         MSGSUP = 32000
C                                       Fill CATBLK.
C                                       Set units.
         CALL CHR2H (8, 'JY      ', 1, CATH(KHBUN))
C                                       Random para. types.
         DO 60 I = 1,NRAN
            INDEX = KHPTP + (I-1)*2
            CALL CHR2H (8, IRAN(I), 1, CATH(INDEX))
 60         CONTINUE
         CATBLK(KIPCN) = NRAN
C                                       Rotations.
         CALL RFILL (NAXIS, 0.0, CATR(KRCRT))
C                                       Increments.
         CALL RFILL (NAXIS, 0.0, CATR(KRCIC))
         CALL RFILL (NAXIS, 1.0, CATR(KRCRP))
         CATR(KRCIC)   = 1.0
         CATR(KRCIC+1) = 1.0
         CATD(KDCRV)   = 1.0D0
         CATD(KDCRV+1) = 1.0D0
C       IF THE NON STANDARD BASELINES ARE INCLUDED WE MUST
C       SET THE STOKES AXIS VALUES TO 'XX', 'YY', 'XY', 'YX'
         IF (DONOTST.GT.0.0) THEN
            CATR(KRCIC+1) = -1.0
            CATD(KDCRV+1) = -5.0D0
         END IF
C                                       Epoch.
         CATR(KREPO) = 1950.0
C                                       Axis types.
         DO 70 I = 1,NAXIS
            INDEX = KHCTP + (I-1)*2
            CALL CHR2H (8, ICOOR(I), 1, CATR(INDEX))
 70         CONTINUE
         CATBLK(KIDIM) = NAXIS
C                                       Axis dimensions.
         CATBLK(KINAX) = 3
         CATBLK(KINAX+1) = 4
         CATBLK(KINAX+2) = 1
         CATBLK(KINAX+3) = 1
         CATBLK(KINAX+4) = 1
C                                       Blank telescope,instrument,
C                                       observer.
         CALL CHR2H (8, 'WSRT    ', 1, CATH(KHTEL))
         CALL CHR2H (8, '        ', 1, CATH(KHINS))
         CALL CHR2H (8, '        ', 1, CATH(KHOBS))
c                                        enable level 6 & 7 messages.
         MSGSUP = 0
C                                       Set file type to 'UV'
         CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
C
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('UVPARM: ERROR',I3,' OBTAINING INPUT PARAMETERS ')
 1010 FORMAT ('UVPARM: INTAPE =',I7,' ILLEGAL')
 1012 FORMAT ('CLASSNAME: ',2A4,' NOT ALLOWED FOR DOALL = ',F3.1)
 1013 FORMAT ('CLASSNAME RENAMED TO: ',2A4)
 1015 FORMAT ('TAPE JOBS ARE NOT ALLOWED IN BATCH')
 1020 FORMAT ('ERROR',I7,' OPENING TAPE')
 1022 FORMAT (' About to skip ', I3, ' tape files, including labels')
 1025 FORMAT ('ERROR',I7,' POSITIONING TAPE')
      END
C
C
C
      SUBROUTINE VISIN (JERR)
C----------------------------------------------------------------------
C  VISIN call SRT2SC to read a Dwingeloo format UV tape into
C  a scratch file.  It then reads the information back, sorting
C  out polarizations and bands and writes it back into an
C  AIPS catalogued file.
C
C  Inputs (via COMMON /SRCINF/):
C  NAMEIN(2)        C*4  Source name
C  TAPEIN           I*2  Tape drive number
C  NFILES           I*2  Label on tape to read
C  BBAND            I*2  Which bands to read
C  EBAND            I*2
C  IBAND            I*2
C
C  from /BUFRS/
C  BUFF0(256)      I*2  Work buffer
C  BUFF1(4610)      R*4  Work buffer
C  BUFF2(4610)      R*4  Work buffer (for 2nd file if DONOTST=3)
C
C  Output:
C  JERR             I*2  Return error code.
C                        0 = OK
C                        5 = Did not find requested data.
C                        8 = I/O error.
C                       11 = OPEN I/O error.
C
C  Programmer: WaWa et al.
C  Last revision March 1989 for tapio
C----------------------------------------------------------------------
      INTEGER JERR
      INTEGER ERROR, IERR
      INTEGER   MJAD(2), LENBU, NUMANT,
     *   SZBUFF,  IFIL, NRPARM, KPOL, NOFF,
     *   BLKOFF, IB, HI, SBUFF(256,4,128), DELETE
      INTEGER   DUPWGT(91), F1OR2
      CHARACTER NATWT*2
      INTEGER   INDEX, INDX(2), KIND(2), IOCNT(2), NIOUT(2)
      INTEGER   BASDUP(91), NTEL, ITEL, JTEL, I
      INTEGER   IBASNO, NWTEL, NOTEL, NBAND, JBAND, CBAND
      INTEGER   IPT, NPT, NPOL, ICOR, ICOFF, IOFF, NCOR, IPOL
      INTEGER   V1, V2, BLKOF
C
C  new common TOHIST (in CWIN.INC / DWIN.inc)
C  I*4: NVISFL(2); R*4: CLAOUT1(2),CLAOUT2(2);
C  I*2: LUN1,LUN2,IND1,IND2,CCNO1,CCNO2,DISOUT1,DISOUT2
C
      LOGICAL   EQUAL, REWT, BADDAT
      CHARACTER*48 PNAME1, PNAME2
      REAL   BAS, COSHA, SINHA, HA, COSDEC, SINDEC, WT,
     *       COSPHI, SINPHI, U, V, W, DAYRAT, FREQ
      REAL   POLM(4,4,5), QUVR(4), QUVI(4),XCNT(2),YREC(2)
      DOUBLE PRECISION XMJAD, TIMFAC, XDAT, STFAC,
     *       GST0, IATDAY , TWOPI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA V1, V2 / 0, 0/, NRPARM/5/, DELETE /-32768/
      DATA LUN1, LENBU, BLKOF /16, 128, 1/
      DATA LUN2 /17/
      DATA POLM /1.,1.,1.,1.,  -1.,-1.,1.,1.,
     *           1.,-1.,1.,-1., -1.,1.,1.,-1.,
     *           2.,0.,0.,2.,   -2.,0.,0.,2.,
     *           0.,-2.,-2.,0.,  0.,2.,-2.,0.,
     *           1.,0.,0.,0.,    0.,1.,0.,0.,
     *           0.,0.,1.,0.,    0.,0.,0.,1.,
     *           2.,2.,0.,0.,   -2.,2.,0.,0.,
     *           0.,0.,0.,0.,    0.,0.,0.,0.,
     *           1.,0.,0.,0.,    0.,0.,0.,1.,
     *           0.,1.,0.,0.,    0.,0.,1.,0./
C        ADDED DIAGONAL MATRIX TO COPE WITH WSRT IQUV LINOBS
C        TAPES. USE POLC = 11 TO IDENTIFY THEM
C        ADDED MATRIX TO CONVERT WSRT XX, XY, YX, YY TO
C        VLA RR, LL, RL, LR PSEUDO FORMAT.  19 SEP 85 / JMH
      DATA NTEL /14/
c                               init. value of dupwgt is 1 (not 0 )
      DATA BASDUP /91*1/, DUPWGT /91*1/
      DATA CCNO1,CCNO2 / 0, 0/
      DATA IATDAY / 0.99726956637D0 /, TWOPI / 6.283185308/
C----------------------------------------------------------------------
      TIMFAC = 1.0D0 / 65536.0D0
      JERR = 0
      REWT = UVWTFN(1:2) .EQ. 'NA'
      JERR = 5
      CALL SRT2SC ( ERROR)
      IF (ERROR.NE.0 .OR. DOLIST) GO TO 999
      CATBLK(KIGCN) = WVSMAX
      CALL CHR2H (4, 'WSRT', 1, CATH(KHTEL))
      CATD(KDBSC) = 1.0D0
      CATD(KDBZE) = 0.0D0
C                                       IAT days/LST radians
C         DAYRAT = .99726956637D0 / TWOPI
      DAYRAT = IATDAY / TWOPI
C                                       Get sort order.
      CALL CHR2H (2, 'BT', 1, CATBLK(KITYP))
C                                       Get RA
      CATD(KDCRV+3) =  RA1
C                                       Get Declination.
      CATD(KDCRV+4) = DEC1
      NBAND = 1 + (EBAND - BBAND)/IBAND
      CATBLK(KINAX+2) = NBAND
C                                       Get bandwidth (Hz)
      IF (NBAND .EQ. 1) THEN
         CATR(KRCIC+2) = BANDW
      ELSE
         CATR(KRCIC+2) = BFREQ(2) - BFREQ(1)
      ENDIF
      CATD(KDCRV+2) = BFREQ(1)
C
      SINDEC = SIN(TWOPI*DEC0/360.)
      COSDEC = COS(TWOPI*DEC0/360.)
      SINPHI = SIN(PHIROT)
      COSPHI = COS(PHIROT)
C                                       Check for duplicate baselines
      DO 120 ITEL = 1, NRINT-1
         DO 110 JTEL = ITEL+1, NRINT
            IF (10..LT.ABS(ABS(BASLIN(JTEL))-ABS(BASLIN(ITEL))))
     *         GO TO 110
            BASDUP(ITEL) = BASDUP(ITEL) + 1
            BASDUP(JTEL) = BASDUP(JTEL) + 1
 110     CONTINUE
         IF (.NOT. REWT .OR. DONOTST.GT.0.0) GO TO 120
            DUPWGT(ITEL) = BASDUP(ITEL)
 120     CONTINUE
C
C ITEL is in the range [1..NRINT-1] so ...
        IF (.NOT. REWT .OR. DONOTST.GT.0.0) GO TO 130
            DUPWGT(NRINT) = BASDUP(NRINT)
 130   CONTINUE
       FREQ = CATD(KDCRV+2) * 1.0D-9
C                                       Write message to user who is
C                                       waiting breathlessly to see if
C                                       reset epoch
      CATR(KREPO) = 1900. + WDATE(1)
C                                       Get number of polarizations.
      NCOR = NRPOL
      CATBLK(KINAX+1) = NCOR
      NVISFL(1)  = 0
      IOCNT(1) = 0
      CATBLK(KIIMS) = 0
      CALL UVCREA (DISOUT1, CCNO1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1050) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
         END IF
C                                        Construct output file name.
      CALL ZPHFIL ('UV', DISOUT1, CCNO1, 1, PNAME1, IERR)
C                                       Initilize vis. count.
      XCNT(1) = 0.0
C
C                                       Do the same for the 2nd output
C                                       output file (class: 'UVNORD')
      IF (DONOTST.EQ.3.0) THEN
         IOCNT(2) = 0
         NVISFL(2) = 0
         CATBLK(KIGCN) = WVSMAX
         CALL CHR2H (6, CLAOUT2, KHIMCO, CATH(KHIMC))
         CALL UVCREA (DISOUT2, CCNO2, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1050) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 999
         END IF
C                                        Construct output file name.
         CALL ZPHFIL ('UV', DISOUT2, CCNO2, 1, PNAME2, IERR)
         XCNT(2) = 0.0
C                                        reset out class
         CALL CHR2H (6,  CLAOUT1, KHIMCO, CATH(KHIMC))
      END IF

C Init DOGBLK1 and DOGBLK2, these are only used when DONOTST=3.
C It's not reallly necessary to use test on DONOTST, because that is
C also done in the routine PTBLK, but it emphasizes the fact that
C DOGBLK and the routine PTBLK are only needed when 2 output files
C are sharing CATBLK
C
      IF (DONOTST.EQ.3) THEN
         CALL PTBLK (1, 'READ')
         CALL PTBLK (2, 'READ')
      END IF
C                                       Set up ANTENNA file
      CALL PTBLK (1, 'WRIT')
      CALL FILEINFO(1)
      CALL DUMPAN (NTEL, JERR)
C                                        CATBLK(K2EXT) altered by DUMPAN
      CALL PTBLK (1, 'READ')
      IF (DONOTST.GE.3) THEN
         CALL PTBLK (2, 'WRIT')
         CALL FILEINFO(2)
         CALL DUMPAN (NTEL, JERR)
         CALL PTBLK (2, 'READ')
      END IF
C                                       Set output rec length R*4 words
      LREC = NRPARM+CATBLK(KINAX)*CATBLK(KINAX+1)*CATBLK(KINAX+2)*
     *   CATBLK(KINAX+3)*MAX0 (1, CATBLK(KINAX+4))
C                                       Open output file.
      CALL ZOPEN (LUN1, IND1, DISOUT1, PNAME1,
     *          .TRUE.,.TRUE.,.TRUE., IERR)
      IF (IERR.EQ.0) GO TO 420
         WRITE (MSGTXT, 1410) IERR,PNAME1
         CALL MSGWRT (8)
         JERR = 11
         GO TO 999
C                                       Initilize output file.
 420  NVIS = 32760 ** 2
      SZBUFF = 9220
         do while (lenbu * lrec .ge. (szbuff/2))
            lenbu = lenbu/2
         enddo
      NIOUT(1) = LENBU
      CALL UVINIT ('WRIT', LUN1, IND1, NVIS, V1, LREC, NIOUT(1),
     *      SZBUFF, BUFF1, BLKOF, KIND(1), IERR)
      IF (IERR.EQ.0) GO TO 430
         WRITE (MSGTXT, 1420) IERR,PNAME1
         CALL MSGWRT (8)
         JERR = 11
         GO TO 999
 430  YREC(1) = 0.0
C                                       If DONOTST=3 then open the
C                                       second output file.
      IF (DONOTST.EQ.3.0) THEN
         CALL ZOPEN (LUN2, IND2, DISOUT2, PNAME2,
     *          .TRUE.,.TRUE.,.TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1410) IERR,PNAME2
            CALL MSGWRT (8)
            JERR = 11
            GO TO 999
         END IF
C                                Initilize output file.
         NVIS = 32760 ** 2
         NIOUT(2) = LENBU
         SZBUFF = 9220
         CALL UVINIT ('WRIT', LUN2, IND2, NVIS, V2, LREC, NIOUT(2),
     *           SZBUFF, BUFF2, BLKOF, KIND(2), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1420) IERR,PNAME2
            CALL MSGWRT (8)
            JERR = 11
            GO TO 999
         END IF
         YREC(2) = 0.0
      END IF
      JERR = 8
C
C                                       VIS records.
C                                       Set up polariz. matrix
      IF ((POLC.EQ.9) .OR. (POLC.EQ.10)) GO TO 440
         WRITE (MSGTXT, 1440) POLC
         CALL MSGWRT (8)
         JERR = 20
         GO TO 999
 440  CONTINUE
      KPOL = POLC - 8
C
C       SET THE SCALING FACTORS AND POL MATRIX PARAMETERS
C
      SC = 0.25
      NOFF = 1
      IF (DONOTST.GT.0.0) GOTO 442
      IF (NRPOL.EQ.4) GOTO 445
      IF (NRPOL.EQ.2) THEN
         KPOL = 4
C                               OFFSET FOR INDEXING IN STRTRC IN LOOP 510
         NOFF = 3
         GOTO 450
      END IF
      IF (NRPOL.EQ.1) THEN
         SC = 1.0 / FLOAT (KPOL)
         GOTO 450
      END IF
442   IF (DONOTST.EQ.3. .OR.  DONOTST.EQ.1. .OR.
     *      DONOTST.EQ.2. .AND. NRPOL.EQ.2) THEN
         KPOL = 3
         NOFF = 3
         SC = 1.0
         GOTO 450
      END IF
      IF (DONOTST.EQ.2.0) THEN
         KPOL = 5
         SC = 1.0
         GOTO 450
      END IF
445   IF (NSTOKES.EQ.1) THEN
         KPOL = 3
         SC = 1.0
      END IF
450   CONTINUE
C
C
C        BEGIN LOOPING THROUGH DATA TO CONSTRUCT OUTPUT VISIBILITIES
C
      DO 600 ITEL = 1, NRINT
C                                  is this a redundant baseline ?
C                                  YES, write to file 1 ['UVRDUN']
C                                  NO,  write to file 2 ['UVNORD']
C                                  if DONOTST<>3 always to file 1
         IF (BASDUP(ITEL).GT.1 .OR. DONOTST.LT.3.0) THEN
            F1OR2 = 1
            CLAOUT(1:6) = CLAOUT1
         ELSE
            F1OR2 = 2
            CLAOUT(1:6) = CLAOUT2
         END IF
C
C        get the telesc.nrs from ITEL, these only use of these ## is
C        to write a message to the user about the tel. configuration.
C
         IBASNO = BASNO(ITEL) + 0.1
         NWTEL  = IBASNO/256
         NOTEL  = IBASNO - NWTEL*256
C
         BLKOFF = 0
         NPT    = NDATP(ITEL)
         HI     = 257
         WT     = IINT(ITEL)
C                       baseline in lambdas, not seconds, for 1st freq. only
         BAS = BASLIN(ITEL) * BFREQ(1)

C                                  Pseudo uniform weight
C         IF (REWT) WT =  WT * BAS / (BASLIN(1) * DUPWGT(ITEL))
C        Changed: average redundant baselines always (not only if DOALL<0)
C        (if doall<0 only 0A & 9C, 0B & 9D are equal). MHW 881111
         IF (REWT) WT =  WT * BAS / (BASLIN(1) * BASDUP(ITEL))
         DO 580 IPT = 1, NPT
            IF (HI.LT.257) GO TO 515
               DO 511 JBAND = 1, NBAND
                  DO 510 IPOL = 1, NRPOL
                     NPOL = 1 + NOFF * (IPOL - 1)
C                               Find initial block number for data
C                               given interf. no., polariz., freq.
                     IB = BLKOFF + STRTRC(ITEL,NPOL,JBAND)
                     CALL ZFIO ('READ', LUNDS, DINDS, IB,
     *                  SBUFF(1,IPOL,JBAND), IERR)
                      IF (IERR.EQ.0) GO TO 510
                         WRITE (MSGTXT, 1520) IERR, IB
                         CALL MSGWRT (8)
                         JERR = 20
                         GO TO 999
 510              CONTINUE
 511           CONTINUE
               BLKOFF = BLKOFF + 1
               HI = 1
 515        CONTINUE
C                                  Check for fully flagged record
            BADDAT = .FALSE.
            DO 521 JBAND = 1, NBAND
               DO 520 IPOL = 1, NRPOL
                  BADDAT = BADDAT .OR.
     *                  (SBUFF(HI,IPOL,JBAND).NE.DELETE)
     *                  .OR. (SBUFF(HI+1,IPOL,JBAND).NE.DELETE)
               IF (BADDAT) GO TO 522
 520           CONTINUE
 521        CONTINUE
            GO TO 580
 522        CONTINUE
C                                       Update vis. count and check
            XCNT(F1OR2) = XCNT(F1OR2) + 1.0
            IF (XCNT(F1OR2).GT.WVSMAX) GO TO 900
C                                       Where to put data in output buffer
            INDX(F1OR2)  = KIND(F1OR2) + IOCNT(F1OR2)*LREC
            IOCNT(F1OR2) = IOCNT(F1OR2) + 1
            INDEX        = INDX(F1OR2)
C
            HA    = SHA(ITEL) + (IPT - 1) * DHA(ITEL)
            SINHA = SIN(HA)
            COSHA = COS(HA)
C                                       Random parameters - U,V,W
            U = -BAS * COSHA
            V = -BAS * SINHA * SINDEC
            W =  BAS * SINHA * COSDEC
C
            IF (F1OR2.EQ.1) THEN
C                                       Precess U&V with PHIROT
               BUFF1(INDEX)   = U + V * SINPHI
               BUFF1(INDEX+1) =-U * SINPHI + V
               BUFF1(INDEX+2) = W
C                                       Baseline
               BUFF1(INDEX+3) = BASNO(ITEL)
C                                        Time
               BUFF1(INDEX+4) = DAYFR + DAYRAT * (HA - SHA(ITEL))
            ELSE
               BUFF2(INDEX)   = U + V * SINPHI
               BUFF2(INDEX+1) =-U * SINPHI + V
               BUFF2(INDEX+2) = W
               BUFF2(INDEX+3) = BASNO(ITEL)
               BUFF2(INDEX+4) = DAYFR + DAYRAT * (HA - SHA(ITEL))
            END IF
C                                       Set further random parameters
C                                       here. NRPARM = # ran. parm.
C                                       Visibilities.
c                                       Loop over freq.
            DO 560 JBAND = 1, NBAND
C                                       Scramble polarization
C                                       default weight
            WTD = WT
C                                       Scramble polarization
            DO 550 IPOL = 1, NCOR
               QUVR(IPOL) = 0.
               QUVI(IPOL) = 0.
               DO 530 ICOR = 1, NCOR
C                                       check for bad point, this freq only
                  IF ((SBUFF(HI,ICOR,JBAND) .EQ. DELETE) .OR.
     *                (SBUFF(HI+1,ICOR,JBAND) .EQ. DELETE)) WTD =-WT
                  ICOFF = 1 + NOFF * (ICOR - 1)
                  QUVR(IPOL) = QUVR(IPOL) + POLM(ICOR,IPOL,KPOL)*SC*
     *               (PSCAL(ITEL,ICOFF,JBAND)*SBUFF(HI,ICOR,JBAND)
     *               +POFF(ITEL,ICOFF,JBAND))
                  QUVI(IPOL) = QUVI(IPOL) + POLM(ICOR,IPOL,KPOL)*SC*
     *               (PSCAL(ITEL,ICOFF,JBAND)*SBUFF(HI+1,ICOR,JBAND)
     *               +POFF(ITEL,ICOFF,JBAND))
 530           CONTINUE
               IOFF = NRPARM+ CATBLK(KINAX)*(IPOL-1 + NCOR*(JBAND-1))
               IF (F1OR2.EQ.1) THEN
                  BUFF1(INDEX+IOFF)   = QUVR(IPOL)
                  BUFF1(INDEX+IOFF+1) = QUVI(IPOL)
                  BUFF1(INDEX+IOFF+2) = WTD
               ELSE
                  BUFF2(INDEX+IOFF)   = QUVR(IPOL)
                  BUFF2(INDEX+IOFF+1) = QUVI(IPOL)
                  BUFF2(INDEX+IOFF+2) = WTD
               END IF
 550        CONTINUE
 560        CONTINUE
C                                       See if output buffer is full.
            IF (IOCNT(F1OR2).LT.NIOUT(F1OR2)) GO TO 580
C                                       Write output buffer(F1ORF2).
               NIOUT(F1OR2) = IOCNT(F1OR2)
               IOCNT(F1OR2) = 0
               YREC(F1OR2)  = YREC(F1OR2) + 1.0
               IF (F1OR2.EQ.1) THEN
                  CALL UVDISK
     *              ('WRIT',LUN1,IND1,BUFF1,NIOUT(1),KIND(1),IERR)
               ELSE
                  CALL UVDISK
     *              ('WRIT',LUN2,IND2,BUFF2,NIOUT(2),KIND(2),IERR)
               END IF
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT, 1510) IERR, F1OR2, YREC(F1OR2)
                  CALL MSGWRT (8)
                  JERR = 11
                  GO TO 999
               END IF
C                                       Update vis. count.
               NVISFL(F1OR2) = NVISFL(F1OR2) + LENBU
C
C                                       Loop back for next record.
 580        HI = HI + 2
 600     CONTINUE
C                                       Finished; cleanup and go home.
C                                       Finish vis. count.
 900     NVISFL(1) = NVISFL(1) + IOCNT(1)
C                                       Check if no data.
         IF (NVISFL(1).NE.0) GO TO 905
            JERR = 15
            GO TO 999
C                                       Finish write and close files.
 905     YREC(1)  = YREC(1) + 1.0
         NIOUT(1) = -IOCNT(1)
         CALL UVDISK
     *       ('FLSH', LUN1, IND1, BUFF1, NIOUT(1), KIND(1), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1510) IERR, 1, YREC(1)
            CALL MSGWRT (8)
         END IF
C                                       Cleanup 2nd file if DONOTST=3
         IF (DONOTST.EQ.3.0) THEN
            NVISFL(2) = NVISFL(2) + IOCNT(2)
            YREC(2)  = YREC(2) + 1.0
C                                        Finish write and close files.
            NIOUT(2) = -IOCNT(2)
            CALL UVDISK
     *           ('FLSH', LUN2, IND2, BUFF2, NIOUT(2), KIND(2), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1510) IERR, 2, YREC(2)
               CALL MSGWRT (8)
            END IF
         END IF
C                                       Shrink 1st file if necessary
C                                       Put vis count in CATBLK
C
         CATBLK(KIGCN) = NVISFL(1)
C                                        Write message about how many vis.
         WRITE (MSGTXT, 1920) 1, NVISFL(1)
         CALL MSGWRT (5)
C                                       Close output file.
         CALL ZCLOSE (LUN1, IND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1921) IERR
            CALL MSGWRT (6)
         ELSE
            JERR = 0
         END IF
C                                       If 2nd file is used, shrink and
C                                       and close this one too.
C The catalogue is not updated by ZCLOSE so it's no use to put
C information in CATBLK that is not saved. The correct number of
C visib. and the class name is put in CATBLK by a call to subr.
C FILEINFO and/or subr. PTBLK ...........
C and then C the catalogue is updated by a call to UVHIS
C or CATIO.
C                                init for UVHIS or CATIO
         CALL FILEINFO(1)
         CALL PTBLK (1, 'WRIT')
         CALL UVHIS
C                                CATBLK(K2EXT) altered by UVHIS
         CALL PTBLK (1, 'READ')
         IF (DONOTST.EQ.3.0) THEN
            CALL FILEINFO(2)
            CALL PTBLK (2, 'WRIT')
            CALL UVHIS
            CALL PTBLK (2, 'READ')
         END IF
C
         IF (DONOTST.EQ.3) THEN
C                                Write message about how many vis.
            WRITE (MSGTXT, 1920) 2, NVISFL(2)
            CALL MSGWRT (5)
            CALL ZCLOSE (LUN2, IND2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1921) IERR
               CALL MSGWRT (6)
            ELSE
               JERR = 0
            END IF
         END IF
  999 CONTINUE
C       close scratch set
      CALL ZCLOSE (LUNDS, DINDS, IERR)
C
C close tape after last band or in case of any error in visin
C
      CALL TAPIO('CLOS', FDVEC, HW, IHND, IERR)
      RETURN
C----------------------------------------------------------------------
 1050 FORMAT ('VISIN: ERROR',I3,' CREATING CATALOGUED OUTPUT FILE')
 1410 FORMAT ('VISIN: ERROR',I3,' OPENING OUTPUT FILE: ',6A4)
 1420 FORMAT ('VISIN: ERROR',I3,' INIT OUTPUT FILE: ',6A4)
 1440 FORMAT ('VISIN: CANT HANDLE DIPOLE AXIS CODE',I6)
 1510 FORMAT ('VISIN: WRITE ERROR',I4,' OUT. FILE:',I4,' BLOCK:',F10.0)
 1520 FORMAT ('VISIN: ZFIO READ ERROR',I7,' BLOCK ',I6)
 1920 FORMAT ('VISIN: Copied (to file:',I2,') ', I10,' vis. records')
 1921 FORMAT ('VISIN: ERROR',I7,' CLOSING CATALOGUED UV FILE')
      END
C
C
C
      SUBROUTINE UVHIS
C----------------------------------------------------------------------
C  UVHIS writes the history file for a uv data base.
C  NAMEIN      R*4(2)  Source name.
C  NVIS      P I*4     Number of visibility points.
C  NAME
C  NAMOUT      C*4(3)  Output file name
C  CLAOUT      C*4(2)  Output file class.
C  SEQOUT      I*2     Output file sequence no.
C
C  Programmer: W. D. Cotton, April 1981.
C----------------------------------------------------------------------
      INTEGER LUN, IERR
      INTEGER   TIME(3), DATE(3)
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /28/
C----------------------------------------------------------------------
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISOUT, CCNO, CATBLK, BUFF0, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (6)
         GO TO 995
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      DATE(1) = DATE(1)-1900
      CALL ZTIME (TIME)
C                                       Write first record.
      WRITE (MSGTXT, 1010) TSKNAM, NLUSER, DATE, TIME
      CALL HIADD (LUN, MSGTXT, BUFF0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Write file name,class,seq.
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISOUT,
     *   LUN, BUFF0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Source name/qualifier/band.
      WRITE (MSGTXT, 1011) TSKNAM, NAMEIN
      CALL HIADD (LUN, MSGTXT, BUFF0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Number of vis.
      WRITE (MSGTXT, 1012) TSKNAM, NVIS
      CALL HIADD (LUN, MSGTXT, BUFF0, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close file
      CALL HICLOS (LUN, .TRUE., BUFF0, IERR)
      GO TO 995
C                                       An error occured.
 980  WRITE (MSGTXT, 1980) IERR
      CALL MSGWRT (5)
 995  CALL CATIO ('UPDT', DISOUT, CCNO, CATBLK, 'CLWR', BUFF0, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT, 1900) IERR
         CALL MSGWRT (6)
C
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('UVHIS: ERROR',I3,' CREATING HISTORY FILE')
 1010 FORMAT (A,'/ DATA BASE CREATED BY USER ',I3,I4,'/',I2,'/',I2,
     *   3X,I2,':',I2,':',I2)
 1011 FORMAT (A,' SOURCE=''',A)
 1012 FORMAT (A,' /NUMBER OF VIS. POINTS = ',I9)
 1900 FORMAT ('ERROR',I7,' UPDATING CATALOG HEADER')
 1980 FORMAT ('UVHIS: ERROR',I3,' WRITING HISTORY FILE')
      END
C
      SUBROUTINE FILEINFO(FILENUMBER)
C----------------------------------------------------------------------
C  this routine fills DISOUT,CCNO,NVIS with DISOUT<i>,
C  CCNO<i> ....   where <i> stands for the file number 1 or 2.
C  argument:
C          FILENUMBER  I*2     File number 1 or 2 (2 only if DONOTST=3).
C----------------------------------------------------------------------
      INTEGER FILENUMBER
      INCLUDE   'WSLOD.INC'
      INCLUDE   'INCS:DCAT.INC'
      INCLUDE   'INCS:DHDR.INC'
C                                   The information is passed
C                                   through the common block FINFO
C----------------------------------------------------------------------
C                                       File 1 or 2 ?
      IF (FILENUMBER.EQ.1) THEN
         CCNO = CCNO1
         DISOUT = DISOUT1
C              put real number of vis. and the correct class in CATBLK.
         NVIS = NVISFL(1)
         CATBLK(KIGCN) = NVIS
         CALL CHR2H (6,  CLAOUT1, KHIMCO, CATH(KHIMC))
      ELSE
         CCNO = CCNO2
         NVIS = NVISFL(2)
         CATBLK(KIGCN) = NVIS
         DISOUT = DISOUT2
         CALL CHR2H (6,  CLAOUT2, KHIMCO, CATH(KHIMC))
      END IF
      RETURN
      END
C---------------------------------------------------------------------
C
      SUBROUTINE PTBLK (FILENUMBER, OPCODE)
C----------------------------------------------------------------------
C performs a similar task as the subroutine fileinfo, but only for
C CATBLK (KIEXT):CATBLK (KIEXT+KIEXTN). This pt. of the catalogue block
C must be unique for the two files, so this routine is only necessary
C if DONOTST EQ 3.0.
C arguments:
C              FILENUMBER  I*2     filenumber 1 or 2
C              OPCODE      R*4     'READ' : CATBLK(KIEXT) --> DOGBLK<i>
C                                  'WRIT' : DOGBLK<i> --> CATBLK(KIEXT)
C                                           where <i> is FILENUMBER.
C----------------------------------------------------------------------
      INTEGER FILENUMBER
      CHARACTER*4    OPCODE
      INCLUDE   'WSLOD.INC'
      INCLUDE   'INCS:DCAT.INC'
      INCLUDE   'INCS:DHDR.INC'
C----------------------------------------------------------------------
C                                        this routine is only necessary
C                                        if DONOTST = 3.
      IF (DONOTST.LE.2.0) GOTO 999
C
      IF (FILENUMBER .EQ. 2) THEN
         IF (OPCODE .EQ. 'READ') CALL ICOPY(20, K2EXT,CATBLK,1,DOGBLK2)
         IF (OPCODE .EQ. 'WRIT') CALL ICOPY(20,1,DOGBLK2,K2EXT,CATBLK)
      ELSE
         IF (OPCODE .EQ. 'READ') CALL ICOPY(20,K2EXT,CATBLK,1,DOGBLK1)
         IF (OPCODE .EQ. 'WRIT') CALL ICOPY(20,1,DOGBLK1,K2EXT,CATBLK)
      END IF
C
  999 RETURN
      END
C---------------------------------------------------------------------
C
      SUBROUTINE ICOPY (NTOCPY, SPOS1, IBUFF1, SPOS2, IBUFF2)
C---------------------------------------------------------------------
C   copies NTOCPY INTEGER   values from IBUFF1, starting at SPOS1 to
C   IBUFF2, starting at SPOS2.
C---------------------------------------------------------------------
      INTEGER I, NTOCPY, SPOS1, SPOS2
      INTEGER   IBUFF1(1), IBUFF2(1)
C---------------------------------------------------------------------
      DO 100 I=1, NTOCPY
         IBUFF2 (SPOS2 + I - 1) = IBUFF1 (SPOS1 + I - 1)
  100 CONTINUE
      RETURN
      END
C---------------------------------------------------------------------
C
C
      SUBROUTINE SRT2SC ( ERROR)
C---------------------------------------------------------------------
C     Read in a Dwingeloo visibility tape and store the data
C     internally
C---------------------------------------------------------------------
      INTEGER ERROR
      INTEGER    I, II, ITEL, ISET, IPOL, ITER, IBLK, N
      INTEGER    NWTEL, NOTEL, IRTYPE
      INTEGER    NREC, HI, FI, NBYTES, LX, LY
      INTEGER    IFND, IHND
      INTEGER    MSH, NTEL, PTS, TPTS, MDB, FWVAR, XNREC
      INTEGER    NDB, NDBPS, SCRLEN
      REAL       XFREQ, EPVAR, SCAL, EXP
      INTEGER    BLKCNT, JERR
      DOUBLE PRECISION     JTIME,JTIM0,JUDAY,DPJTIM, HAST, HAEND, HAFTIM, DAYRAT
      DOUBLE PRECISION     DPVAR, TWOM16
      DOUBLE PRECISION     PREC(9), NUTA(9), ABER(3), VNPA(3), DVNPA(3)
      REAL       WLAT, WLON,  TWOPI, SPDLIT, DFREQ
      INTEGER    POST(14), TDATE(6), UDAY, UTIME
      INTEGER    NRSTS, LOH, LSC, LRCRD, PHBLL, NFREQ, NTOT, PROJN
      INTEGER    SFREQ, PBAND
      INTEGER    ITAPVR, NOBAND, NGSET(4), NITER, NBAND, JBAND, CBAND
      LOGICAL    DOALL, NOTIBM, l1, l2, l3
      LOGICAL    INTOK, READSH,SYS52
      CHARACTER*2 CHTEMP, DATYP, TTYPE(5), CPOL(4)
      CHARACTER*1 CSTOK(4)
      CHARACTER*12 SOURSE
      CHARACTER*8 CDATE
      CHARACTER*4 BACK
      CHARACTER*6 VOLUME
      CHARACTER*3 TAPTYP(2)
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /TAPCOM/ NREC, HI, FI, IHND, NBYTE
      DATA CPOL /'XX','XY','YX','YY'/, TTYPE /'FD','OH','SC','SH',
     *   'IH'/
      DATA CSTOK /'I','Q','U','V'/
      DATA TAPTYP /'IBM', 'VAX'/
      DATA NTEL /14/
      DATA SPDLIT /2.997925E+8/, JTIM0 /2440000.D0/
C-----------------------------------------------------------------------
      TWOPI  = 2.*3.1415926
      TWOM16 = 2.D0**(-16)
C                                     ratio iat day to sideral day
      DAYRAT = .99726956637D0
C                                     Read File Description Record
      NBYTES = 3840
      LX = 1920
      LY = 9999
      LUNDS = 27
      SPDLIT = 1./SPDLIT
C                                     Loop back here on index function
 20   CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
      IF (JERR.NE.0) GO TO 960
      IRTYPE = 1
      CALL ZC8CL (2, 3, HW(IFND), CHTEMP)
      NOTIBM = (CHTEMP .EQ. TTYPE(IRTYPE))
C If not ok then maybe the tape isn't in ASCII format, try to
C read it in EBCDIC format ...
         IF (.NOT. NOTIBM) THEN
            CALL ZCHEBC(2, CHTEMP, CHTEMP)
            IF (CHTEMP .NE. TTYPE(IRTYPE)) THEN
               GOTO 950
            ENDIF
         END IF
C
C If tape is in IBM/EBCDIC format, swap the order of bytes in HW
C attention: not the whole buffer, only the part that will be used !!
C all I*2 numbers are now correct ...
C
C                                          Get volume and label #
      IF (DOLIST) THEN
         CALL ZC8CL (6, 105, HW(IFND), VOLUME)
         IF (.NOT. NOTIBM) CALL ZCHEBC(6, VOLUME, VOLUME)
         NFILES = XFILES + .01
         I = 1
         IF (NOTIBM) I = 2
         WRITE (MSGTXT , 1068) NFILES, VOLUME, TAPTYP(I)
         CALL MSGWRT(5)
      ENDIF
C
      CALL TI16IL(48, HW(IFND), CW, NOTIBM)
      CALL TI32IL(1, HW(17+IFND), MSH ,NOTIBM)
      ITAPVR = CW(13)
      IF (.NOT.DOLIST.AND.ITAPVR.LT.6 .AND. DONOTST.GT.0.0) THEN
          WRITE (MSGTXT, 1073) ITAPVR
          CALL MSGWRT (5)
      END IF
      LRCRD  = CW(14)
      PHBLL  = CW(15)
C                   Total # data blocks; # data blocks per set
c   just changed 24+ifind to 25+ifnd?
      CALL TI32IL (1, HW(25+IFND), MDB, NOTIBM)
C                       Replace MSH with 4 ?
c      CALL TI32IL (32, HW, HW, NOTIBM)
      NDBPS  = 1 + (MDB - 1)/MSH
C                                       Create scratch file
      NBAND  = 1 + (EBAND - BBAND)/IBAND
C                                            Read first OH block
      CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GO TO 960
      CALL TI16IL(335, HW(IFND), CW, NOTIBM)
      CALL ZC8CL (2, 3, HW(IFND), CHTEMP)
      IF (.NOT.NOTIBM)  THEN
         CALL ZCHEBC(2, CHTEMP, CHTEMP)
      ENDIF
      IRTYPE = 2
      IF (CHTEMP .NE. TTYPE(IRTYPE)) GO TO 950
      CALL TI32IL (1, HW(2+IFND), LOH, NOTIBM)
      SYS52 = (CW(33) .EQ. 52)
      ERROR = 5
C                               Source name on tape
      CALL ZC8CL (12, 1, HW(IFND+7), SOURSE)
      IF (.NOT.NOTIBM)  THEN
         CALL ZCHEBC(12, SOURSE, SOURSE)
      ENDIF
      IF (NAMEIN(1:4) .EQ. '    '.OR. DOLIST) NAMEIN = SOURSE
C                                          sourse name correct ?
      IF (NAMEIN .NE. SOURSE) THEN
         WRITE (MSGTXT, 1050) SOURSE, NAMEIN
         GO TO 980
      END IF
      CALL CHR2H (8, NAMEIN, 1, CATH(KHOBJ))
C                                       Default NAMOUT=NAMEIN.IQUAL
C                                       (If IQUAL.le.0 NAMOUT=NAMEIN)
      IF (NAMOUT .EQ. ' ') NAMOUT = NAMEIN
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
C                                       Let project number be observer
      PROJN = CW(14)
      WRITE (CDATE, 1065) PROJN
      CALL CHR2H (8, CDATE, 1, CATH(KHOBS))
C                                       Let backend be instrument
      CALL ZC8CL (4, 173, HW(IFND), BACK)
      IF (.NOT.NOTIBM)  THEN
         CALL ZCHEBC(4, BACK, BACK)
      ENDIF
      CALL CHR2H (8, BACK//'    ', 1, CATH(KHINS))
C
      DO 70 I = 1, 6
 70      TDATE(I) = CW(I+24)
C                                       These are (1) Epoch - 1900
C                                                 (2) Obs. Year - 1900
C                                                 (3) Obs. Month
C                                                 (4--6) Obs Day Hour Min (MET)
C                                       UT day# and time in 10s
      UDAY  = CW(9)
      UTIME = CW(10)
C                                       Convert start of year + hh/mm/ss to JD
      WDATE(1) = TDATE(2)
      WDATE(4) = UTIME/360.
      UTIME    = UTIME - WDATE(4)*360.
      WDATE(5) = UTIME/6.
C                                       Seconds, make approx. correction
C                                       for IAT - UT.
      WDATE(6) = 24. + 0.65 * (WDATE(1) - WDATE(1)) +
     &          10. * (UTIME - 6. * WDATE(5)) + 0.5
      WDATE(2) = 1
      WDATE(3) = 0
      CALL DAT2JD (WDATE, JTIME)
C                                       Add UT day number
      JTIME = JTIME + UDAY
C                                       Convert back to civil system
      CALL GREG (JTIME, CDATE)
      DAYFR = (WDATE(6) + 60. * WDATE(5) + 3600. * WDATE(4))/86400.
C                                       TOTAL number of channels
      NTOT = CW(77)
C                                       Number of frequencies observed
      NFREQ = CW(78)
C                                       Channel separation
      CALL ZINR32 (CW(75), BANDW, NOTIBM)
C                                       FREQ in in kHz
      DFREQ = 1.e3 * BANDW/NFREQ
C       DFREQ = 0.1 * CW(79)  ! works for DLB, not for DCB
C               One tape block (MDB) is 3840 true bytes, is 1920 integers
C               which take 3840 AIPS bytes to store is 3840/256 disk blocks
C               The number of tape blocks in NDBPS * number of sets needed
C               Fudge upward by 2 because short 32 point WSRT tape records
C               don't pack well into aips 64 point blocks.
C                                       Actual # freq on tape
      NFREQ = CW(335)
      SCRLEN = 2. * NBAND * (1./FLOAT(NFREQ)) * MDB * (3840./256.)
      IF (.NOT. DOLIST) THEN
         CALL SCRFIL (SCRLEN, JERR)
            IF (JERR.EQ.0) GO TO 75
               ERROR = 2
               GO TO 999
      ENDIF
c
 75   CALL ZINR64(CW(101), HAST, NOTIBM)
      CALL ZINR64(CW(105), HAEND, NOTIBM)
      HAST = HAST * 360.D0
      HAEND = HAEND * 360.D0
      CALL CHR2H (8, CDATE, 1, CATH(KHDOB))
      CALL ZINR64 (CW(65), DPVAR, NOTIBM)
         RA1 = 360.D0 * DPVAR
      CALL ZINR64 (CW(69), DPVAR, NOTIBM)
         DEC1= 360.D0 * DPVAR
         IF (RA1 .LT. 0.D0) RA1 = 360.D0+RA1
      CALL ZINR64 (CW(93), DPVAR, NOTIBM)
         DEC0 = 360.D0 * DPVAR
      CALL ZINR64 (CW(89), DPVAR, NOTIBM)
         RA0  = 360.D0 * DPVAR
         IF (RA0.LT.0.D0) RA0 = 360.D0+RA0
      NRPOL = CW(80)
C
C     Set NRPOL to 2 if we want redundant baselines because we
C     will eventually skip the XY and YX data.
C
      IF ((DONOTST.EQ.3.0 .OR. DONOTST.EQ.1.0).AND. NRPOL.EQ.4)
     *     NRPOL = 2
C                                       THIS IS THE # STANDARD BASELINES
C                                       SO DO NOT USE
C                                       nrint is multiple of 16, so
C                                       will be 96 i.s.o. 91 (DBX-type,
C                                       tape version 6)
      NRINT = CW(81)
      CALL ZINR32 (CW(75), BANDW, NOTIBM)
      BANDW = BANDW * 1.E+6
      POLC  = CW(74)
C                                       IF 1 THEN IQUV ARE ON TAPE
      NSTOKES = CW(185)
      NRSTS   = CW(334)
C
      IF (DONOTST.GT.3.0) GOTO 985
      IF (.NOT.DOLIST.AND.DONOTST.GT.0.0) THEN
            WRITE (MSGTXT, 1072) DONOTST
            CALL MSGWRT (5)
      END IF
      IF (POLC.NE.10 .AND. DONOTST.GT.0.0) THEN
         WRITE (MSGTXT, 1075)
         CALL MSGWRT (5)
      END IF
C                              Telescope positions in meters, find minimum
         CALL TI32IL (NTEL, HW(75+IFND), POST, NOTIBM)
      DO 80 ITEL = 1, NTEL
         BASLM(ITEL) = POST(ITEL) * TWOM16
 80   CONTINUE
      IF (.NOT.DOLIST) THEN
         DPVAR = BASLM(11) - BASLM(10)
         WRITE (MSGTXT, 1600) SOURSE, HAST, HAEND, DPVAR
         CALL MSGWRT(5)
         WRITE (MSGTXT, 1005) BBAND, EBAND, IBAND
         CALL MSGWRT(5)
      ENDIF
C                             Read 2nd OH rec
      CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GO TO 960
C
C                              Read System Calibration Rec
      CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GO TO 960
      CALL ZC8CL (2, 3, HW(IFND), CHTEMP)
      IF (.NOT.NOTIBM)  THEN
         CALL ZCHEBC(2, CHTEMP, CHTEMP)
      ENDIF
      IRTYPE = 3
      IF (CHTEMP .NE. TTYPE(IRTYPE)) GO TO 950
      CALL TI16IL(224, HW(IFND), CW, NOTIBM)
      CALL TI32IL (1, HW(2+IFND), LSC, NOTIBM)
C                                            Latitute and Longitude
      CALL ZINR32(CW(217), WLAT, NOTIBM)
      CALL ZINR32 (CW(219), WLON, NOTIBM)
C                                          Precesion rotation angle
      CALL ZINR32 (CW(223), PHIROT, NOTIBM)
      IF (.NOT.DOLIST.AND.PHIROT .EQ. 0.0) THEN
         MSGTXT = 'SRT2SC:THERE IS NO PRECESION ROTATION ON THIS TAPE'
         CALL MSGWRT(5)
         ENDIF
      PHIROT = PHIROT * TWOPI
C                                          Read 2nd SC record
      CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GO TO 960
C                                       Read 3rd & 4th SC records
C                                       for tape version 6
      IF (ITAPVR.GE.6) THEN
         CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GOTO 960
         CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
         IF (JERR.NE.0) GOTO 960
      END IF
C                                          Loop through sets
      BLKCNT = 1
      READSH = .FALSE.
C                                          1st desired band
      CBAND  = BBAND
      JBAND  = 1
C                                       NRSTS replaces MSH
      DO 500 ISET = 1, NRSTS
C                                          Find next set header
         CHTEMP = '  '
         IRTYPE = 4
         DO WHILE (CHTEMP .NE. TTYPE(IRTYPE))
            CALL TAPIO ('READ', FDVEC, HW, IFND, JERR)
               IF (JERR.NE.0) GO TO 960
            CALL ZC8CL (2, 3, HW(IFND), CHTEMP)
            IF (.NOT.NOTIBM)  THEN
               CALL ZCHEBC(2, CHTEMP, CHTEMP)
            ENDIF
         ENDDO
         CALL TI16IL(20, HW(IFND), CW, NOTIBM)
C
C                       Find which polarization (or is it an IF set?)
         CALL ZC8CL (2, 31, HW(IFND),  DATYP)
         IF (.NOT.NOTIBM)  THEN
            CALL ZCHEBC(2, DATYP, DATYP)
         ENDIF
         POLC = CW(17)
         DOALL = (DONOTST.EQ.3.0 .OR. DONOTST.EQ.1.0)

C
C          If we want the non standard baselines we should read the
C          number of interferometers from the set header to include
C          them in the count
C
         IF (DOALL) NRINT = CW(18)
C                                       Do we want this set?
         IF (DATYP.EQ.'IF') GOTO 500
C                               For LIST, get PTS and FREQ and quit
         IF (DOLIST) THEN
            CALL TI32IL (1, HW(9+IFND),  PTS, NOTIBM)
            CALL TI32IL (1,HW(6+IFND), FWVAR, NOTIBM)
            BFREQ(1) = FWVAR * TWOM16
            GO TO 600
         ENDIF
C
C      If we want redundant baselines we should skip XY and YX
C
         IF (((DATYP.EQ.'XY') .OR. (DATYP.EQ.'YX'))
     *          .AND. DOALL) GOTO 500
         IF (ITAPVR .LE. 5) THEN
c                                Tape version 5
            DO 111 N=1,NRPOL
 111           NGSET(N) = NRPOL*NBAND + N
            DO 112 N=1,NRPOL
               IF (ISET.EQ.NGSET(N)) GOTO 115
 112           CONTINUE
C                               Skip rest of tape
               IF (ISET.GT.NGSET(NRPOL)) GOTO 510
            GOTO 500
            END IF
         IF (ITAPVR .GE. 6) THEN
C                                                Tape version 6
            NOBAND = CW(11)
            IF (NOBAND.LT.CBAND) THEN
C                                                Read some more tape
               GOTO 500
            ELSE IF (NOBAND.EQ.CBAND) THEN
C                                               process this set
               GOTO 115
            ELSE IF (NOBAND.GT.EBAND) THEN
C                                               skip rest of tape
               GOTO 510
            ELSE
C                                               NOBAND>CBAND
               CBAND = CBAND + IBAND
               JBAND = JBAND + 1
               IF (CBAND .EQ. NOBAND) GOTO 115
               GOTO 500
            ENDIF
         END IF
 115     CONTINUE
         DO 118 IPOL = 1, 4
            IF (DATYP.EQ.CPOL(IPOL)) GO TO 120
 118     CONTINUE
            WRITE (MSGTXT, 1250) DATYP
            GO TO 980
C                                              poltype ok
 120     CONTINUE
         CALL TI32IL (1, HW(2+IFND), XNREC, NOTIBM)
         CALL TI32IL (1, HW(9+IFND),   PTS, NOTIBM)
         IF (PTS.EQ.0) PTS=57600
C
C   Shall we print something?
         PBAND = MOD (JBAND, 5)
         IF ((PBAND .EQ. 0) .OR. (CBAND+IBAND .GT. EBAND)) THEN
            IF (PBAND .EQ. 0) PBAND = 5
            PBAND = 1 - PBAND
            WRITE (MSGTXT, 1071) DATYP, (CBAND+I*IBAND, I=PBAND,0)
            CALL MSGWRT (5)
         ENDIF
C
C                                        Freq in Hz
         CALL TI32IL (1,HW(6+IFND), FWVAR, NOTIBM)
         BFREQ(JBAND) = FWVAR * (1.D6) * TWOM16
         XFREQ = 1.E-9 * BFREQ(JBAND)
C    what is at HW(15+IHND) ??? this cannot be the BANDW
C    so it's better to take the old value of BANDW
C    (that was computed some 140 lines ago)
C         CALL TI32IL (1, HW(7+IFND), FWVAR, NOTIBM)
C         FWVAR = CW(15)
C         BANDW = FLOAT (FWVAR) * 100.0
         TPTS = PTS
         WVSMAX = 1.5 * PTS
C                                         Loop through interforms
         NITER = 0
         NREC = XNREC
         DO 400 ITER = 1, NRINT
C                                       IH record starts new block in
C                                       format 6 tapes.
C
C set NREC >> 30 to force GTREC to make a call to TAPIO
            IF (ITER.EQ.1 .AND. ITAPVR.GE.6)  nrec=32000
C                                         IH Record (not registered
C                                          in block for format <=5)
            CALL GTREC (NOTIBM, JERR)
            IF (JERR.NE.0) GOTO 960
            IRTYPE = 5
            CALL ZC8CL (2, 3, HW(FI), CHTEMP)
            IF (.NOT.NOTIBM)  THEN
               CALL ZCHEBC(2, CHTEMP, CHTEMP)
            ENDIF
            IF (CHTEMP .NE. TTYPE(IRTYPE)) THEN
               WRITE (MSGTXT, 1140) CHTEMP
               ERROR = 1
               GO TO 980
            END IF
            NWTEL = CW(HI+12)
            NOTEL = CW(HI+13)
C do all interferometers or only standard interferometers ...
            IF (DOALL) THEN
               INTOK = .TRUE.
            ELSE
               CALL INTCHK (NWTEL, NOTEL, INTOK)
            END IF
C
C                                   ignore IH-blocks 92..96 and
C                                   telescp. numbers >= 15
            NWTEL = NWTEL + 1
            NOTEL = NOTEL + 1
            INTOK = (INTOK) .AND. (MAX(NWTEL,NOTEL) .LE. 14)
            IF (INTOK) THEN
               NITER = NITER + 1
C                               Only calculate baselines etc. for 1st band
               IF (JBAND .EQ. 1) THEN
                  BASNO(NITER) = NOTEL + 256. * NWTEL
                  BASLIN(NITER) = POST(NOTEL) - POST(NWTEL)
C                                    Baseline in light seconds
                  BASLIN(NITER) = BASLIN(NITER)*TWOM16*SPDLIT
C                                   Start hour angle
                  CALL ZINR32 (CW(HI+18), EPVAR, NOTIBM)
                  SHA(NITER) = EPVAR * TWOPI
C                                   Delta hour angle
                  CALL ZINR32 (CW(HI+22), EPVAR, NOTIBM)
                  DHA(NITER) = EPVAR * TWOPI
C                                   Calc integration time from HA-step
                  IF (SYS52) IINT(NITER)=NINT(EPVAR*360*240*DAYRAT)
C                                   Number of data points
                  NDATP(NITER) = CW(HI+25)
C                                   The rest for each band
               ENDIF
C
C                                  Scaling factor 0.005 to get from w.u.
C                                  to Jy.
C                                  Appropriate factors for polarization
C                                  are taken care of in VISIN
               SCAL = FLOAT( CW(HI+28) ) * 0.005
               EXP  = 2.0**CW(HI+27)
               PSCAL(NITER,IPOL,JBAND) = SCAL*EXP
               POFF (NITER,IPOL,JBAND) = FLOAT( CW(HI+29))
C                                  Integration time in sec
               IF (.NOT.SYS52) IINT(NITER) = CW(HI+54)
               STRTRC(NITER, IPOL,JBAND) = BLKCNT
C                                  Got parameters for wanted baselines
            END IF
C                                               find # data blocks
            NDB = CW(HI+24)
            DO 300 IBLK = 1, NDB
               CALL GTREC (NOTIBM, JERR)
               IF (JERR.NE.0) GO TO 960
C                                               Don't write red.ifm's
                  IF (INTOK) THEN
                     CALL PUTBLK (IBLK, NDB, BLKCNT, CW, HI, JERR)
                     IF (JERR.NE.0) THEN
                        ERROR = 3
                        GO TO 999
                     END IF
                  END IF
 300        CONTINUE
 400     CONTINUE
C
C       UPDATE THE NUMBER OF INTERFEROMETERS IN CASE THE REDUNDANT
C       BASELINES WERE SKIPPED, or if nrint contains some dummy
C       ih-blocks (92..96)
C
        NRINT = NITER
C                                        do NEXT SET
 500  CONTINUE
C
 510  ERROR = 0
C                                       write info about tapeversion,
C                                       # of set, # of intfm's .. e.a.
         NRINT = NITER
         WRITE (MSGTXT, 1200) SOURSE, XFREQ, TPTS
         CALL MSGWRT (5)
      GO TO 999
C                                       Skip three files for listimg
 600  CONTINUE
      DPVAR = BASLM(11) - BASLM(10)
      WDATE(1) = WDATE(1) + 1900
      WRITE (MSGTXT, 1600) SOURSE, HAST, HAEND, DPVAR
      CALL MSGWRT(5)
      WRITE (MSGTXT, 1601) BFREQ(1), RA1, DEC1, WDATE(1)
      CALL MSGWRT(5)
      WRITE (MSGTXT, 1602) WDATE(4), WDATE(3), WDATE(2),
     *          WDATE(5), WDATE(6)
      CALL MSGWRT(5)
      WRITE(MSGTXT, 1603) NFREQ, DFREQ, NRPOL, BACK
      CALL MSGWRT(5)
      NFILES = 3
      CALL TAPIO ('CLOS', FDVEC(1), HW, IFND, IERR)
      CALL TAPIO ('OPRD', FDVEC(1), HW, IFND, IERR)
      CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IERR)
      XFILES = XFILES + 1.
      IF (IERR .EQ. 0) GO TO 20
      ERROR = 0
      GOTO 999
 950  CONTINUE
      ERROR = 1
      WRITE (MSGTXT, 1950) CHTEMP, TTYPE(IRTYPE)
      GO TO 980
 960  ERROR = 1
      IF (JERR.EQ.8) THEN
         GOTO 999
      ELSE IF (JERR .EQ. 4 .AND. DOLIST) THEN
         MSGTXT = '    End of Tape '
         ERROR = 0
      ELSE
         WRITE (MSGTXT, 1960) JERR
      ENDIF
      GO TO 980
 985  WRITE (MSGTXT, 1076) DONOTST
      ERROR = 1
      GOTO 980
 980  CALL MSGWRT (8)
      GO TO 999
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('1st Band#: ',I3,', Last Band#: ',I3,', Band Inc: ',I3)
 1050 FORMAT('SRT2SC:FOUND ',A,' INSTEAD OF ',A)
 1065 FORMAT('W',I7)
 1068 FORMAT ('  Label ',I4, ' Volume ',A,'  ',A,' Format Tape')
 1071 FORMAT('For Polarization ', A, ' Found Bands ', 5(I3,','))
 1072 FORMAT('SRT2SC: DOALL =',F3.0,'! SEE EXPLAIN WSLOD ',
     *        'FOR INFO ON HOW TO GET I, Q, U AND V !')
 1073 FORMAT('SRT2SC: TAPE VERSION',I5,' HAS NO REDUNDANT',
     * ' BASELINES!')
 1074 FORMAT(I2,'/',I2,'/',I2)
 1075 FORMAT('SRT2SC: NO PARALLEL DIPOLES! REDUNDANT BASELINE',
     *        ' INFORMATION NOT USEFUL!')
 1076 FORMAT('SRT2SC: INVALID VALUE FOR DOALL: ',F3.0)
 1140 FORMAT('SRT2SC: CANT FIND IH BLOCK ',A)
 1200 FORMAT('For ',A, ' at ',F9.6,' Ghz found',
     :  I9,' vis pts')
 1250 FORMAT('SRT2SC: WHAT IS POLTYPE ',A)
 1600 FORMAT(A,' HA=', F6.2,' ->',F6.2, ' Baseline 9A=',F5.1, ' meters')
 1601 FORMAT('  Chan 0=', F9.3, ' MHz, RA=', F9.5,
     *   '  Dec=', F9.5, ' (' , I4, ')' )
 1602 FORMAT('  Obsdate ',I2,'/',I2,'/',I2,' Obstime ',I2,':',I2)
 1603 FORMAT(I5,' Freq. Chans. at ',F7.1,' kHz  '
     *,I2,' Pol. Channels ', ' Backend ',A)
 1950 FORMAT('SRT2SC:FOUND RECORD ',A,' INSTEAD OF ',A)
 1960 FORMAT('SRT2SC: TAPE READ ERROR ',I6)
C
      END
C
C
C
      SUBROUTINE GTREC (NOTIBM, IERR)
C-----------------------------------------------------------------------
C     While reading IH and DB records, decide if we
C     need a new block, and get it if necessary.  In
C     any case, set some pointers.
C     If NOTIBM is true, the bytes on the tape are in VAX order
C     (small byte first), otherwise IBM order (bigendian)
C     IERR                   0 : ok
C                         1..6 : error in TAPIO
C                            8 : error in counting # of records
C-----------------------------------------------------------------------
      INTEGER IERR
      INTEGER   NREC, HI, FI, NBYTES, DI, IHND
      LOGICAL   NOTIBM
      INCLUDE 'WSLOD.INC'
      INCLUDE   'INCS:DMSG.INC'
      COMMON    /TAPCOM/ NREC, HI, FI, IHND, NBYTES
C-----------------------------------------------------------------------
C If NREC < 0 then there is something wrong.
C
      IF (NREC .LE. 0) THEN
         WRITE (MSGTXT, 1100) NREC
         CALL MSGWRT (8)
         IERR = 8
         GOTO 999
      END IF
C
      IF (NREC.LT.30) GO TO 100
         CALL TAPIO ('READ', FDVEC, HW, IHND, JERR)
         IF (IERR.NE.0) GO TO 999
         CALL TI16IL (1920, HW(IHND), CW, NOTIBM)
         NREC = 0
         FI = IHND - 32
 100     NREC = NREC + 1
C                       HI is pointer to first integer in record in CW
c                       FI is pointer to unflipped data in HW
      HI = 64 * (NREC - 1) + 1
      FI = FI + 32
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1100 FORMAT('GETRC: LOST COUNT OF # OF RECORDS, NREC: ',I7)
      END
C
C
C
      SUBROUTINE SCRFIL ( MDB, IERR)
C-----------------------------------------------------------------------
C      Create a scratch file to hold native WSRT visibilities
C      MDB should be the number of 256 "AIPS BYTE" records we need
C-----------------------------------------------------------------------
      INTEGER MDB, IERR
      INTEGER   ISEQ
      INTEGER   WBUFF(256), ERRCNT
      INTEGER MDB4, ASIZE
      CHARACTER*48 PNAM
      LOGICAL   NOMAP, NOWAIT, EXCL
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (WBUFF, BUFF1)
C
      DATA ISEQ /0/, IVER /1/
      DATA NOMAP, NOWAIT, EXCL /.FALSE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C               91 is a margin of safety, probably not needed
      MDB4 = 91 + MDB
C               convert to AIPS bytes
      MDB4 = 256 * MDB
      IVOL = 0
      ICNO = 1
      SNO  = 1
      CALL CATDIR ('OPEN', IVOL, ICNO, 'WSLOD       ',
     *          'SCRACH', SNO, 'SC',
     *   NLUSER, STAT, WBUFF, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT, 1110) IERR
         CALL MSGWRT (8)
         GO TO 999
 110  CONTINUE
      CALL ZPHFIL ('SC', IVOL, ICNO, IVER, PNAM, IERR)
C                                       Error count
      ERRCNT = 0
 112  CALL ZCREAT (IVOL, PNAM, MDB4, NOMAP, ASIZE, IERR)
         IF (IERR.EQ.0) GO TO 120
C         If no space on disk 1 try on disk 2
      IF (IERR.NE.3) GOTO 114
      IF (IVOL .GE. 2) GOTO 115
      CALL UNSCR
      ERRCNT = ERRCNT + 1
      IVOL = 2
      CALL ZPHFIL ('SC', IVOL, ICNO, IVER, PNAM, IERR)
      GOTO 112
C                                      Problem with dup name?
  114 CONTINUE
         IF ((IERR.NE.1) .OR. (ERRCNT.GT.0)) GO TO 115
            CALL UNSCR
            ERRCNT = ERRCNT + 1
            GO TO 112
 115        WRITE (MSGTXT, 1100) IERR
            CALL MSGWRT (8)
            GO TO 999
 120  CONTINUE
      CALL CATDIR ('CSTA', IVOL, ICNO, NAMPAK, CLSPAK, SNO, 'SC',
     *   NLUSER, 'CLWR', WBUFF, IERR)
      CALL ZOPEN (LUNDS, DINDS, IVOL,PNAM,NOMAP,EXCL,NOWAIT,  IERR)
         IF (IERR.EQ.0) GO TO 999
           WRITE (MSGTXT, 1200) IERR
           CALL MSGWRT (8)
 999  CONTINUE
      RETURN
C----------------------------------------------------------------------
 1100 FORMAT(' ZCREAT ERROR FOR SCRATCH ',I6)
 1110 FORMAT(' SCRATCH CATDIR ERROR ',I6)
 1200 FORMAT(' ZOPEN ERROR FOR SCRATCH ',I6)
      END
C
C
C
      SUBROUTINE PUTBLK (IBLK, NBLK, BLKCNT, BUFF, OFFS, IERR)
C-----------------------------------------------------------------------
C     Write out the Westerbork Data records, with blocking
C        into scratch file
C-----------------------------------------------------------------------
      INTEGER   IBLK, BLKCNT, BUFF(*), OFFS, IERR, IB
      INTEGER   NBLK
      INTEGER   HOLD(256)
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
C
C-----------------------------------------------------------------------
      IB = MOD (IBLK, 4)
      IF (IB.EQ.0) IB = 4
      CALL COPY(64, BUFF(OFFS), HOLD(1+64*(IB-1)))
      IF ((IB.NE.4) .AND. (IBLK.NE.NBLK)) GO TO 999
         CALL ZFIO ('WRIT', LUNDS, DINDS, BLKCNT, HOLD, IERR)
         IF (IERR.EQ.0) GO TO 800
            WRITE (MSGTXT, 1000) IERR
            CALL MSGWRT (8)
            GO TO 999
 800     CONTINUE
      BLKCNT = BLKCNT + 1
 999  CONTINUE
      RETURN
C---------------------------------------------------------------------
 1000 FORMAT(' ZFIO ERROR IN PUTBLK ',I6)
      END
C
C
C
      SUBROUTINE UNSCR
C---------------------------------------------------------------------
C      Get rid of scratch files
C---------------------------------------------------------------------
      INTEGER   ISEQ, IERR
      INTEGER   WBUFF(256)
      CHARACTER*48 PNAM
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (WBUFF, BUFF1)
C     DATA IVER /1/, ISEQ /1/
C---------------------------------------------------------------------
C     IVOL = 1
   10 CALL ZPHFIL ('SC', IVOL, ICNO, IVER, PNAM, IERR)
      CALL CATDIR ('CLOS', IVOL, ICNO, 'WSLOD       ',
     *  'SCRACH', 1,
     * 'SC', NLUSER, WBUFF, WBUFF, IERR)
      CALL ZDESTR (IVOL, PNAM, IERR)
        IF (IERR.EQ.0) GO TO 900
        IF (IVOL.LT.2) THEN
                IVOL = IVOL + 1
                GO TO 10
        END IF
        IF (IERR.EQ.1) THEN
                WRITE (MSGTXT, 1000)
                CALL MSGWRT (8)
                GO TO 900
        END IF
        IF (IERR.EQ.2) THEN
                WRITE (MSGTXT, 1001)
                CALL MSGWRT (8)
                GO TO 900
        END IF
        IF (IERR .GE. 3) THEN
                WRITE (MSGTXT, 1002)
                CALL MSGWRT (8)
        END IF
900     CONTINUE
        RETURN
C----------------------------------------------------------------------
 1000 FORMAT('UNSCR: COULD NOT FIND SCRATCH FILES FOR DESTROY')
 1001 FORMAT('UNSCR: COULD NOT DESTROY SCRATCH FILES')
 1002 FORMAT('UNSCR: ERROR',I3,' FROM ZDESTR')
C
      END
C
      SUBROUTINE DUMPAN (IMAXANT, IERR)
C-----------------------------------------------------------------------
C   DUMPAN creates an AN file and writes the AN common data to it.
C   Inputs: IMAXANT I*2   # antennas
C   Output: IERR    I*2   error code of TABINI/TABAN
C-----------------------------------------------------------------------
      INTEGER IMAXANT, IERR
      INTEGER   IABUF(512), NUMV(12)
      INTEGER   ANVER, FINDA, I, JERR, LUNA
      REAL      ABUF(256)
      CHARACTER*4 NAMSTA(14)
      DOUBLE PRECISION    A8BUF(128)
      DOUBLE PRECISION    DEG2RD, FREQ, JD
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'WSLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C
      EQUIVALENCE (IABUF, ABUF, A8BUF)
C
      DATA ANVER, LUNA / 2, 30/
      DATA NUMV /8,3,0,1,1,1,1,1,3,1,1,3/
      DATA NAMSTA /'0   ','1   ','2   ','3   ','4   ','5   ','6   ',
     *             '7   ','8   ','9   ','A   ','B   ','C   ','D   '/
C-----------------------------------------------------------------------
      IERR  = 0
      DEG2RD= 3.141592653589793D0 / 180.0D0
C                                       Setup for AN table initization
      NUMORB = 0
      NOPCAL = 3
C                                       Position of the earth's pole
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
C                                       UT1UTC, IATUTC not used
      UT1UTC = 0.0
      IATUTC = 0.0
C                                       Array name
      ANAME = 'WSRT'
C                                       Array center (rel to center of
C                                       earth), not used.
      ARRAYC(1) = 3828440.6D0
      ARRAYC(2) = -445226.0D0
      ARRAYC(3) = 5064923.1D0
C                                       GST0 is not used ...
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD)
C     CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
      GSTIA0   = 0.0D0
      DEGPDY   = 0.0D0
      FREQ     = CATD(KDCRV+2)
      ANFQID   = -1
C                                       Number of values in each column
C                                       in the AN table.
      CALL COPY (12, NUMV, ANNUMV)
C                                       Create/init file
      CALL ANTINI ('WRIT', IABUF, DISOUT, CCNO, ANVER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, FREQ,
     *   RDATE, POLRXY, UT1UTC, IATUTC, ANAME, NUMORB, NOPCAL, ANFQID,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
      ANNAME = 'WSRT'
      STAXOF    = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA  = 0
      MNTSTA = 1
      POLAA  = 0.0
      POLAB  = 90.0
      CALL RFILL (3, 0.0, POLCA)
      CALL RFILL (3, 0.0, POLCB)
      POLTYA = 'X'
      POLTYB = 'Y'
      IF (IMAXANT.LE.0) GO TO 30
C                                       AN records
         DO 10 I = 1,IMAXANT
            STAXYZ(1) = -0.1155160D0 * BASLM(I)
            STAXYZ(2) = -0.9933060D0 * BASLM(I)
            STAXYZ(3) = 0.0D0
            ANNAME(5:8) = NAMSTA(I)
            NOSTA  = I
            IANRNO = I
            CALL TABAN ('WRIT', IABUF, IANRNO, ANKOLS, ANNUMV,
     *             ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *             POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 10      CONTINUE
C                                       Close/update AN file.
 30   CALL TABIO ('CLOS', 1, IANRNO, ANNAME, IABUF, IERR)
      GO TO 999
C                                       Error
 990  CALL ZCLOSE (LUNA, FINDA, JERR)
C
 999  RETURN
      END
C
C
C
      SUBROUTINE INTCHK (NFIX,NMOV,IOK)
C----------------------------------------------------------------------
C     This routine checks whether INFNR is a WSRT standard inter-
C     ferometer or not.  If so, IOK is .TRUE., if not IOK is .FALSE.
C----------------------------------------------------------------------
      INTEGER   NFIX,NMOV
      LOGICAL   IOK
C
C
      IOK = .FALSE.
      IF (NFIX.LT.0 .OR. NFIX.GT.13) RETURN
      IF (NMOV.LT.0 .OR. NMOV.GT.13) RETURN
      IF (NFIX.LE.9 .AND. NMOV.GE.10) IOK = .TRUE.
      RETURN
      END
C
      SUBROUTINE TI16IL(NW, INBUF, OUTBUF, NOTIBM)
C----------------------------------------------------------------------
C Convert tape format I*2 integers to local integers.  Possible
C tape formats are IBM (2-s complement, big byte first) and VAX
C (2-s complement, small byte first)
c
C  INPUTS:
C               NW      I       Number of words to convert
C               INBUF   I*2(*)  Input array of tape integers
C               NOTIBM  L       .true. -> VAX format tape
C                               .false.-> IBM format
C  OUTPUT:      OUTBUF  I       Local Integers.  Should not be same
C                               as INBUF
C------------------------------------------------------------------------
      INTEGER   NW, INBUF(*), OUTBUF(*)
      LOGICAL   NOTIBM
      INTEGER   TEMBYT
      INCLUDE 'INCS:DDCH.INC'
C                                       Store BYTFLP
      TEMBYT  = BYTFLP
C                                       Fool ZI16IL about whether
C                                       We need to flip
      IF ((NOTIBM .AND. (BYTFLP .GT. 0))
     &    .OR. (.NOT.NOTIBM .AND. (BYTFLP .EQ. 0))) THEN
         BYTFLP = 0
      ELSE
         BYTFLP = 3
      ENDIF
C                               With BYTFLP temporarily changed, flip
C                               as necessary
      CALL ZI16IL (NW, 1, INBUF, OUTBUF)
C                               Restore
      BYTFLP = TEMBYT
C
      RETURN
      END
      SUBROUTINE TI32IL(NW, INBUF, OUTBUF, NOTIBM)
C----------------------------------------------------------------------
C Convert tape format I*4 integers to local integers.  Possible
C tape formats are IBM (2-s complement, big byte first) and VAX
C (2-s complement, small byte first)
c
C  INPUTS:
C               NW      I       Number of words to convert
C               INBUF   I*4(*)  Input array of tape integers
C               NOTIBM  L       .true. -> VAX format tape
C                               .false.-> IBM format
C  OUTPUT:      OUTBUF  I       Local Integers.  Can be same as INBUF
C  This version will work if BYTFLP = 0 (no flipping) or =3 (word and
C  byte swap as in VAXes).  It probably won't work for BYTFLP =1 or 2.
C------------------------------------------------------------------------
      INTEGER   NW, INBUF(*), OUTBUF(*)
      LOGICAL   NOTIBM
      INTEGER   TEMBYT
      INCLUDE 'INCS:DDCH.INC'
C                                       Store BYTFLP
      TEMBYT  = BYTFLP
C                                       Fool ZI16IL about whether
C                                       We need to flip
      IF ((NOTIBM .AND. (BYTFLP .GT. 0))
     &    .OR. (.NOT.NOTIBM .AND. (BYTFLP .EQ. 0))) THEN
         BYTFLP = 0
      ELSE
         BYTFLP = 3
      ENDIF
C                               With BYTFLP temporarily changed, flip
C                               as necessary
      CALL ZI32IL (NW, 1, INBUF, OUTBUF)
C                               Restore
      BYTFLP = TEMBYT
C
      RETURN
      END
C
      SUBROUTINE ZINR32 (TINT, LOCAL, FRMVAX)
C----------------------------------------------------------------------
C       Convert a tape single precision floating point number, stored
C       as the least sig. 2 bytes in two local integers.  These
C       are assumed already flipped etc. as necessary by TI16IL.
C----------------------------------------------------------------------
      INTEGER NREAL
      LOGICAL FRMVAX
      INTEGER TINT(2)
      REAL LOCAL
C
      INTEGER SMASK, EXPMSK(2), MNTMSK(2), SIGN, EXPM, M2
      INTEGER MANT, TWO16
      INTEGER RTYPE, ZAND
      REAL TEMP
C
      DATA SMASK /32768/,
     *          EXPMSK /32640, 32512/,
     *          MNTMSK /127, 255/
C      DATA SMASK /Z8000/, EXPMSK /Z7F80/, MNTMSK /Z007F/
      DATA TWO16 /65536/
C      DATA TWO16 /Z00010000/
C
      IF (FRMVAX) THEN
         RTYPE = 1
      ELSE
         RTYPE = 2
      ENDIF
C
      SIGN = ZAND(TINT(1), SMASK)
      EXPM = ZAND(TINT(1), EXPMSK(RTYPE))
      M2   = ZAND(TINT(1), MNTMSK(RTYPE))
      IF (FRMVAX) THEN
C                       7 BIT, 128 BIASED EXPONENT
         EXPM = (EXPM/128) - 128
C                       BINARY POINT IS 8 PLACES TO RIGHT
         EXPM = EXPM - 8
C                       VIRTUAL BIT
          M2 = M2 + 128
       ELSE
C               rIGHT SHIFT EIGHT BITS, SUBTRACT BIAS OF 64, MULTIPLY
C               BY 4 (ibm EXP IS HEXADECIMAL) AND SUBTRACT 8 (BECAUSE
C               MANTISSA M2 IS ALREADY LEFT SHIFTED 8 BITS WRT
C               THE HEXADECIMAL POINT
         EXPM = (EXPM/64) - 264
      ENDIF
      LOCAL = M2 * 2.**EXPM
      MANT = TINT(2)
      IF (MANT .LT. 0)  MANT = MANT + TWO16
      EXPM = EXPM - 16
      LOCAL = LOCAL + MANT*2.**EXPM
      IF (SIGN .NE. 0) LOCAL = -LOCAL
C
      RETURN
      END
C
      SUBROUTINE ZINR64 (TINT, LOCAL, FRMVAX)
C----------------------------------------------------------------------
C       Convert a tape double precision float (VAX or IBM) into local
C       Double precision.  Assume that input is in the least significant
C       two bytes of 4 consecutive local integers, already flipped.
C----------------------------------------------------------------------
C
      LOGICAL FRMVAX
      INTEGER TINT(4)
      DOUBLE PRECISION LOCAL
      INTEGER NREAL
C
      INTEGER SMASK, EXPMSK(2), MNTMSK(2), SIGN, EXPM, M2
      INTEGER MANT, TWO16
      DOUBLE PRECISION TEMP
      INTEGER RTYPE

C
      DATA SMASK /32768/,
     *          EXPMSK /32640,32512/,
     *          MNTMSK /127, 255/
C      DATA SMASK /Z8000/, EXPMSK /Z7F80/, MNTMSK /Z007F/
      DATA TWO16 /65536/
C      DATA TWO16 /Z00010000/
C
      IF ( FRMVAX) THEN
         RTYPE = 1
      ELSE
         RTYPE = 2
      ENDIF
C
      SIGN = ZAND(TINT(1), SMASK)
      EXPM = ZAND(TINT(1), EXPMSK(RTYPE))
      M2   = ZAND(TINT(1), MNTMSK(RTYPE))
      IF (FRMVAX) THEN
         M2 = M2 + 128
         EXPM = (EXPM/128) - 128
         EXPM = EXPM - 8
      ELSE
         EXPM = (EXPM/64) - 264
      ENDIF
      LOCAL = M2 * 2.D0 ** EXPM
      DO I = 2, 4
         MANT = TINT(I)
         IF (MANT .LT. 0) MANT = MANT + TWO16
         EXPM = EXPM - 16
         LOCAL = LOCAL + MANT * 2.**EXPM
      ENDDO
      IF (SIGN .NE. 0) LOCAL = -LOCAL
C
      RETURN
      END
C
      SUBROUTINE ZCHEBC (NR_CH_IN_STRING, CH_IN, CH_OUT)
C-----------------------------------------------------------------------
C
C     Translates a character string from ascii to ebcdic
C
C-----------------------------------------------------------------------
      CHARACTER        CH_IN(*), CH_OUT(*)
      INTEGER          TABLE(0:255), J, NR_CH_IN_STRING, I
      DATA    (TABLE(J) , J=0,63) /
     *     0,    1,    2,    3,   92,    9,   92,  127,
     *   92,   92,   92,   11,   12,   13,   14,   15,
     *   16,   17,   18,   19,   92,   92,    8,   92,
     *   24,   25,   92,   92,   28,   29,   30,   31,
     *   92,   92,   92,   92,   92,   10,   23,   27,
     *   92,   92,   92,   92,   92,    5,    6,    7,
     *   92,   92,   22,   92,   92,   92,   92,    4,
     *   92,   92,   92,   92,   20,   21,   92,   26/
      DATA    (TABLE(J) , J=64,127) /
     *    32,   92,   92,   92,   92,   92,   92,   92,
     *    92,   92,   91,   46,   60,   40,   43,   33,
     *    38,   92,   92,   92,   92,   92,   92,   92,
     *   92,   92,   93,   36,   42,   41,   59,   94,
     *   45,   47,   92,   92,   92,   92,   92,   92,
     *   92,   92,  124,   44,   37,   95,   62,   63,
     *   92,   92,   92,   92,   92,   92,   92,   92,
     *   92,   96,   58,   35,   64,   39,   61,   34/
      DATA    (TABLE(J) , J=128,191) /
     *   92,   97,   98,   99,  100,  101,  102,  103,
     *  104,  105,   92,   92,   92,   92,   92,   92,
     *   92,  106,  107,  108,  109,  110,  111,  112,
     *  113,  114,   92,   92,   92,   92,   92,   92,
     *   92,  126,  115,  116,  117,  118,  119,  120,
     *  121,  122,   92,   92,   92,   92,   92,   92,
     *   92,   92,   92,   92,   92,   92,   92,   92,
     *   92,   92,   92,   92,   92,   92,   92,   92/
      DATA    (TABLE(J) , J=192,255) /
     *  123,   65,   66,   67,   68,   69,   70,   71,
     *   72,   73,   92,   92,   92,   92,   92,   92,
     *  125,   74,   75,   76,   77,   78,   79,   80,
     *   81,   82,   92,   92,   92,   92,   92,   92,
     *   92,   92,   83,   84,   85,   86,   87,   88,
     *   89,   90,   92,   92,   92,   92,   92,   92,
     *   48,   49,   50,   51,   52,   53,   54,   55,
     *   56,   57,   92,   92,   92,   92,   92,  255/
C=======================================================================
       DO I=1,NR_CH_IN_STRING
         CH_OUT(I) = CHAR (TABLE(ICHAR(CH_IN(I))))
      END DO
C
      RETURN
      END
