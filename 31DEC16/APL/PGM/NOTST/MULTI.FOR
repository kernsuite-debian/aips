LOCAL INCLUDE 'MULTI.INC'
C                                       Local include to MULTI
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATBLK(256), SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   JBUFSZ, NUMANT, NUMIF, NUMPOL, CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   NUMAN(513), BUFFC(512), ICLRNO, OLDLOC(20), NEWLOC(20), NSHUFL,
     *   OLDCNO, NEWCNO, CATOLD(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, SRCNAM(30)*16,
     *  HISCRD(10)*64
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XSRCNA(4,30)
      REAL      XSIN, XDISIN, XSOUT, XDISO, APARM(10), BUFF1(UVBFSS),
     *   BUFF2(UVBFSS)
      LOGICAL   PIPELN, ADDFQ
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /CLPRMS/ CATOLD, NUMANT, NUMIF, NUMPOL, CLKOLS, CLNUMV,
     *   NUMAN, BUFFC, ICLRNO, OLDLOC, NEWLOC, NSHUFL,
     *   PIPELN, ADDFQ, NUMHIS, OLDCNO, NEWCNO
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU,
     *   XCLAOU, XSOUT, XDISO, XSRCNA, APARM, SEQIN, SEQOUT, DISKIN,
     *   DISKO
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, SRCNAM, HISCRD
      COMMON /MAPHDR/ CATBLK
LOCAL END
      PROGRAM MULTI
C-----------------------------------------------------------------------
C! Converts a single source uv data file to a multisource file
C# UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000-2001, 2004, 2007-2008, 2012, 2015-2016
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
C   MULTI takes a UV data base in single source format and writes it
C   into a new UV data base in multi-source format.  This is done in
C   three subroutines.
C
C   The first subroutine, CATMOD, reads in the old catalog header and
C   creates a new UV dataset with the header modified to add the SOURCE
C   random parameter to each data record.  It also initializes the SU
C   and CL tables.
C
C   The second subroutine, PASSUV, reads in the UV data from the old UV
C   database, adds the SOURCE random parameter value to each data
C   record, and writes the expanded record to the new database.  It
C   also adds a new line to the CL table for each antenna which is up
C   during each time interval, the length of which is specified in
C   APARM(1).
C
C   The third subroutine, WRTHIS, writes out a history record and copies
C   any tables over to the new database from the old database.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      SOURCES(4,30)  SRCNAM        Name of source
C      APARM(10)      APARM         APARM(1) is time interval for the CL
C                                   table, in minutes.  Default is 1.
C-----------------------------------------------------------------------
      INTEGER  IRET
      CHARACTER PRGM*6
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MULTI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'MULTI '   /
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CATMOD (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL PASSUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL WRTHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CATMOD (PRGN, JERR)
C-----------------------------------------------------------------------
C   CATMOD gets input parameters for MULTI and reads in the header from
C   the input UV database.  It then initializes an output UV database,
C   with the header modified to indicate the addition of the SOURCE
C   random parameter, and initializes the SU and CL tables.
C
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGN*6, UTYPE*2, CHTM8*8, VELTYP*8, VELDEF*8,
     *   SOUNAM*16, CALCOD*4, VRAD*8, VOPT*8, VTPSEL(4)*8, STAT*4
      HOLLERITH CATH(256)
      INTEGER   IROUND, NPARM, DIR, ISUB, LUNT, IVELTY, IERR, SUFQID,
     *   BUFFS(512), VERO, IDSOU, QUAL, SUKOLS(MAXSUC), SUNUMV(MAXSUC),
     *   ISURNO, NSUBA, MAXFLX, I, CAT1(256), CAT2(256), NTERM
      REAL      CATR(256)
      REAL   FLUX(4,MAXIF), GMMOD, POLAR(2)
      DOUBLE PRECISION CATD(128), FREQO(MAXIF), BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, EQUIN, DELDAT, LSRVEL(MAXIF),
     *   LRESTF(MAXIF), PMRA, PMDEC, JD, RRMEA, RDMEA, RRAPP, RDAPP,
     *   DEG2RD, OBSPOS(3), RAOBS, DECOBS
      LOGICAL   T, ISMULT
      INCLUDE 'MULTI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      PARAMETER ( MAXFLX = 4*MAXIF )
      EQUIVALENCE (CATD, CATH, CATR, CATBLK)
      EQUIVALENCE (NSUBA, NUMAN(1))
      DATA T /.TRUE./
      DATA VERO, IDSOU, QUAL /3*1/
      DATA LUNT /27/
      DATA CALCOD /'    '/
      DATA FLUX /MAXFLX*0./
      DATA FREQO, PMRA, PMDEC /MAXIF*0.0,2*0.0/
      DATA LSRVEL, LRESTF /MAXIF*0., MAXIF*0./
      DATA GMMOD /1./
      DATA VRAD, VOPT /'RADIO   ','OPTICAL ' /
      DATA VTPSEL /'LSR     ', 'BARYCENT', 'TOPOCENT', '        '/
      DATA EQUIN, DELDAT /1950.0D0, 1.0D-6/
      DATA OBSPOS, POLAR /3 * 0.0D0, 2 * 0.0/
C-----------------------------------------------------------------------
      ICLRNO = 0
      ISURNO = 0
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.  CHECK
C                                       THIS AGAINST THE INPARM COMMON
C                                       in the local include above!!!
C                                       It was 151, reset now correctly
C                                       Well... 148 was still not right.
C                                       Fixed PPM 1996.09.30
      NPARM = 144
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XSRCNA(1,I), SRCNAM(I))
 20      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
 30   OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
C                                       Get uv header info.
 50   CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      CALL COPY (256, CATBLK, CATOLD)
C                                       Is this already multi-source
      CALL MULSDB (CATBLK, ISMULT)
      IF (ISMULT) THEN
         JERR = 1
         MSGTXT = 'THIS FILE IS ALREADY A MULTI-SOURCE DATABASE'
         GO TO 990
         END IF
C                                        Get number of antennas.
C                                        Do early so as not to lose
C                                        extension file info (after
C                                        call to SOUINI) in CALBLK.
      CALL GETNAN (DISKIN, OLDCNO, CATBLK, LUNT, BUFFC, NUMAN, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.10) THEN
            MSGTXT = 'ERROR IN READING AN TABLE.  QUITTING'
            JERR = 1
            GO TO 990
         ELSE
            MSGTXT = 'NO AN TABLE FOUND.' //
     *         ' ASSUMING 1 SUBARRAY, 10 ANTENNAS'
            CALL MSGWRT (8)
            END IF
         END IF
      IF (NSUBA.LE.1) THEN
         NSUBA = 1
         NUMANT = NUMAN(2)
      ELSE
         NUMANT = 0
         DO 190 ISUB = 2,NSUBA+1
            NUMANT = MAX (NUMANT, NUMAN(ISUB))
 190        CONTINUE
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Check random parameters
C                                       First, save originals
      CALL COPY (256, CATBLK, CAT1)
C                                       RPCHK will alter CATBLK
C                                       so that it represents
C                                       the output file.
      CALL RPCHK (JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'PROBLEM CHECKING RANDOM PARAMETERS'
         GO TO 990
         END IF
C                                       Rerun UVPGET so that
C                                       the output file will be
C                                       the correct size.
      CALL COPY (256, CATBLK, CAT2)
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Create new UV file
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         ELSE IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
            JERR = 1
            GO TO 990
C                                       Recover existing CATBLK
         ELSE
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1080) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
C                                       Transfer the actual sequence
C                                       number to the modified CATBLK:
      CAT2(KIIMS) = CATBLK(KIIMS)
      NEWCNO = CCNO
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                       Restore the original
C                                       CATBLK and rerun UVPGET
C                                       so it represents the
C                                       input data
      CALL COPY (256, CAT1, CATBLK)
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       The restore the
C                                       modified CATBLK because
C                                       it now represents the
C                                       output data. But delete the
C                                       info on extension tables.
      CALL COPY (256, CAT2, CATBLK)
      CALL CATCLR (CATBLK)
C                                       Create new tables
C                                       Get number of IF groups
      IF (JLOCIF.GE.0) THEN
         NUMIF = CATBLK(KINAX+JLOCIF)
         IF (NUMIF.GT.MAXIF) THEN
            MSGTXT = 'EXCEEDS MAXIMUM NUMBER OF IFS.  QUITTING'
            JERR = 1
            GO TO 990
            END IF
      ELSE
         NUMIF = 1
         END IF
C                                        Specify velocities
      IVELTY = CATBLK(KIALT)
      IF (IVELTY.LE.256) THEN
         VELDEF = VOPT
      ELSE
         VELDEF = VRAD
         IVELTY = IVELTY - 256
         END IF
      IF ((IVELTY.LE.0) .OR. (IVELTY.GT.3) ) IVELTY = 4
      VELTYP = VTPSEL(IVELTY)
C                                        Initialize SU table
      SUFQID = -1
      CALL SOUINI ('WRIT', BUFFS, DISKO, CCNO, VERO, CATBLK, LUNT,
     *   NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR IN INITIALIZING SU TABLE.  QUITTING'
         JERR = 1
         GO TO 990
         END IF
C                                        Get parameters for SU table
      SOUNAM = SRCNAM(1)
      IF (SRCNAM(1).EQ.'                ')
     *   CALL H2CHR (8, 1, CATH(KHOBJ), SOUNAM)
      BANDW = CATR(KRCIC+JLOCF)
      EPOCH = CATR(KREPO)
C                                        Calculate Julian date
      CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
      CALL JULDAY (CHTM8, JD)
      RAEPO = CATD(KDCRV+JLOCR)
      DECEPO = CATD(KDCRV+JLOCD)
      RAOBS = CATD(KDORA)
      DECOBS = CATD(KDODE)
C                                        Convert degrees to radians,
C                                        precess coordinates, and
C                                        convert back to degrees
      DEG2RD = ATAN (1.0D0) / 45.
      RRMEA  = RAEPO * DEG2RD
      RDMEA  = DECEPO * DEG2RD
      EQUIN = EPOCH
      DIR = 1
      CALL JPRECS (JD, EQUIN, DELDAT, DIR, T, OBSPOS, POLAR, RRMEA,
     *   RDMEA, RRAPP, RDAPP)
C
C      CALL PRECES (JD, EQUIN, DELDAT, RRMEA, RDMEA, RRAPP, RDAPP, T, F,
C     *   F, ZERO8, ZERO8, ZERO8, F)
      RAAPP  = RRAPP / DEG2RD
      DECAPP = RDAPP / DEG2RD
C                                        Write source info to SU table
      CALL TABSOU ('WRIT', BUFFS, ISURNO, SUKOLS, SUNUMV, IDSOU, SOUNAM,
     *   QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR IN WRITING TO SU TABLE.  QUITTING'
         JERR = 1
         GO TO 990
         END IF
C                                        Flush buffer to write to disk
      CALL TABSOU ('CLOS', BUFFS, ISURNO, SUKOLS, SUNUMV, IDSOU, SOUNAM,
     *   QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR IN CLOSING SU TABLE.  QUITTING'
         JERR = 1
         GO TO 990
         END IF
C                                        Get number of pol'ns per IF
      NUMPOL = CATBLK(KINAX+JLOCS)
      IF (NUMPOL.GT.1) NUMPOL = 2
C                                       IQUV?
      IF (CATD(KDCRV+JLOCS).GT.0.0D0) NUMPOL = 1
      NTERM = 1
      CALL CALINI ('WRIT', BUFFC, DISKO, CCNO, VERO, CATBLK, LUNT,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   JERR)
      IF (JERR.GT.0) THEN
         MSGTXT = 'ERROR IN INITIALIZING CL TABLE.  QUITTING'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATMOD: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1080 FORMAT ('CATMOD: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE PASSUV (IRET)
C-----------------------------------------------------------------------
C   PASSUV obtains UV data one point at a time, and adds a source
C   random parameter to each record.  It also writes out lines to the
C   CL table for each calibration time interval.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C   Programmer = K. R. Lind, June 1987
C-----------------------------------------------------------------------
      CHARACTER OFILE*48, IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECI,
     *   LRECO, ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2,
     *   NRPRMI, NRPRMO, IS, IA, ISUB, INCX, NSUBA, BO, VO, NUMVIS,
     *   XCOUNT
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSUB, MXANTS, MXIFR0, MXIFR1
C                                       MAXSUB = Max. no. subarrays
      PARAMETER (MAXSUB = 50)
      PARAMETER (MXANTS = MAXANT*MAXSUB)
      PARAMETER (MXIFR0 = 25*MAXIF)
      PARAMETER (MXIFR1 =  4*MAXIF)
      INTEGER   ANTUP(MAXANT,MAXSUB), SOURID, REFA(2,MAXIF), FREQID,
     *   IBASE, RNXRET
      REAL      DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      DOUBLE PRECISION    GEODLY(3)
      REAL   TIMEI(MAXSUB), TIMEE(MAXSUB), TINT, TIB, TIE, TID,
     *   IFR, BASEN
      DOUBLE PRECISION TIC
      INCLUDE 'MULTI.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (NSUBA, NUMAN(1))
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA REFA /MAXIF*0, MAXIF*0/
      DATA ANTUP /MXANTS*0/
      DATA SOURID /1/
      DATA TIMEI, TIMEE /MAXSUB*0., MAXSUB*0./
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /MAXIF*0.0, 6*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, DELAY, RATE, WEIGHT /MAXIF*0.0, MAXIF*0.0,
     *   MAXIF*0.0, MAXIF*0.0, MAXIF*0.0, MAXIF*0.0, MAXIF*1.0,
     *   MAXIF*1.0/
      DATA CREAL /MAXIF*1.0, MAXIF*1.0/
      DATA IFR /0.0/
C-----------------------------------------------------------------------
C                                       Set up time interval
      TINT = 1./1440.
      IF ( APARM(1) .GT. 0. ) TINT = APARM(1)*TINT
      FREQID = 1
      IFR = 0.0
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      LRECI = LREC
      NRPRMI = NRPARM
C                                       LRECO = length of output rec.
C                                       Add 1 for SOURCE r.p.
      LRECO = LREC + 1
      NRPRMO = NRPARM + 1
C                                       Add another for FREQSEL
      IF (ADDFQ) THEN
         LRECO = LRECO + 1
         NRPRMO = NRPRMO + 1
         END IF
C                                       If pipeline data have replaced
C                                       FLAG with SOURCE => subtract 1
      IF (PIPELN) THEN
         LRECO = LRECO - 1
         NRPRMO = NRPRMO - 1
         END IF
C                                       Sets up 'OLD' positions for
C                                       'new' r.p's
      IF (.NOT. PIPELN) ILOCSU = NRPRMI
      IF (ADDFQ) ILOCFQ = NRPRMI + 1
C
      NRPARM = NRPRMO
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      NRPARM = NRPRMI
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
      INIO = ILENBU
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
               IBASE = BASEN + 0.1
               ISUB = (BASEN - IBASE) * 100.0 + 1.5
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               ISUB = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            IF (ISUB.EQ.0) ISUB = 1
            ANTUP(IA1,ISUB) = 1
            ANTUP(IA2,ISUB) = 1
            IF (NUMVIS.LE.0) THEN
               TIB = BUFF1(IPTRI+ILOCT)
               TIE = TIB + TINT
               END IF
            IF (TIMEI(ISUB) .LT. TIB ) TIMEI(ISUB) = BUFF1(IPTRI+ILOCT)
            IF (BUFF1(IPTRI+ILOCT) .LT. (TIE-1.16E-6)) GO TO 120
C                                       If at end of record time, write
C                                       out records to CL file and set
C                                       up new record
               DO 115 IS = 1,NSUBA
                  DO 114 IA = 1,NUMAN(IS+1)
                     TIC = (TIMEE(IS) + TIMEI(IS)) * 0.5D0
                     TID = TIMEE(IS) - TIMEI(IS)
                     IF (ANTUP(IA,IS) .EQ. 0 ) GO TO 112
                     CALL TABCAL ('WRIT',BUFFC,ICLRNO,CLKOLS,CLNUMV,
     *                  NUMPOL,NUMIF,TIC,TID,SOURID,IA,IS,FREQID,
     *                  IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *                  CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *                  RATE, WEIGHT, REFA, IRET)
                     IF (IRET.NE.0) GO TO 999
                     ANTUP(IA,IS) = 0
 112                 CONTINUE
 114              CONTINUE
 115           CONTINUE
               TIB = BUFF1(IPTRI+ILOCT)
               TIE = TIB + TINT
               TIMEI(ISUB) = TIB
               ANTUP(IA1,ISUB) = 1
               ANTUP(IA2,ISUB) = 1
 120           TIMEE(ISUB) = BUFF1(IPTRI+ILOCT)
            NUMVIS = NUMVIS + 1
C                                       Copy record and add source parm.
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMI, BUFF1(IPTRI), BUFF2(IPTRO))
            BUFF2(IPTRO+ILOCSU) = 1.
            IF (ADDFQ) BUFF2(IPTRO+ILOCFQ) = 1.
C                                        Switch around the r.p's if
C                                        necessary
            IF (NSHUFL.GT.0) CALL SHUFFL (BUFF2(IPTRO), NRPRMO, OLDLOC,
     *         NEWLOC, NSHUFL)
            CALL RNXUPD (BUFF2(IPTRO), RNXRET)
            CALL RCOPY (LRECO-NRPRMO, BUFF1(IPTRI+NRPRMI),
     *         BUFF2(IPTRO+NRPRMO))
            NIOUT = NIOUT + 1
            IPTRI = IPTRI + LRECI
            IPTRO = IPTRO + LRECO
C                                       Write vis record.
            NRPARM = NRPRMO
            IF (NIOUT.LT.NIOLIM) GO TO 190
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.EQ.0) GO TO 160
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
 160           CONTINUE
               IPTRO = KBIND
               NIOUT = 0
 190        CONTINUE
         GO TO 100
C                                       Final records to CL table
 200     NUMVIS = -1
         DO 204 IS = 1, NSUBA
            DO 203 IA = 1, NUMAN(IS+1)
               TIC = (TIMEE(IS) + TIMEI(IS)) * 0.5D0
               TID = TIMEE(IS) - TIMEI(IS)
               IF ( ANTUP(IA,IS) .EQ. 1 )
     *            CALL TABCAL ('WRIT',BUFFC,ICLRNO,CLKOLS,CLNUMV,
     *            NUMPOL,NUMIF,TIC,TID,SOURID,IA,IS,FREQID,
     *            IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *            RATE, WEIGHT, REFA, IRET)
               IF (IRET.NE.0) GO TO 999
 203        CONTINUE
 204     CONTINUE
         CALL TABCAL ('CLOS',BUFFC,ICLRNO,CLKOLS,CLNUMV,
     *      NUMPOL,NUMIF,TIC,TID,SOURID,IA,IS,FREQID,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *      CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *      RATE, WEIGHT, REFA, IRET)
         IF (IRET.LE.0) GO TO 205
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Finish write
 205  NIOUT = - NIOUT
      NRPARM = NRPRMO
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PASSUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('PASSUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('PASSUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('PASSUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('PASSUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('PASSUV: ERROR',I3,' WRITING CL TABLE')
 1150 FORMAT ('PASSUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE WRTHIS
C-----------------------------------------------------------------------
C   WRTHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(2)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MULTI.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT /2/
      DATA NOTTYP /'SU','CL'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      NUMHIS = 2
      WRITE (HISCRD( 9),1019) SRCNAM(1)
      WRITE (HISCRD(10),1020) APARM(1)
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL //HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 30
         WRITE (MSGTXT,1030)
         CALL MSGWRT (6)
C                                        Update CATBLK.
 30   CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRTHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1019 FORMAT ('SOURCES  ',A16,' / Name of source')
 1020 FORMAT ('APARM    ',F8.3,8X,' / CL time interval (minutes)')
 1030 FORMAT ('WRTHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE RPCHK (IERR)
C-----------------------------------------------------------------------
C  Routine to check that all necessary random parameters are present.
C  Also ensures they are in the correct order.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR
C
      CHARACTER RPCHR(20)*8, TMPCHR(20)*8
      INTEGER   RPLOC(20), I, INDEX, WTOFF, SCLOFF, SRCOFF, FQOFF,
     *   FGOFF, SCPOS, NREPOS, J, NT, JERR, NUMFQ, WTPOS
      HOLLERITH CATH(256)
      REAL      CATR(256)
      LOGICAL   RPSHUF, CMPRES
      INCLUDE 'MULTI.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATH, CATR)
C-----------------------------------------------------------------------
C                                       This gets complicated, its
C                                       much easier for the logic of
C                                       some later tasks if the WEIGHT
C                                       and SCALE random parameters (if
C                                       they exist) are the last two
C                                       in the list, so we have to
C                                       rearrange things. Get the
C                                       indices here and rearrange
C                                       on the fly.
C
C                                       Check if data from the
C                                       'Pipeline'
      PIPELN = .FALSE.
      CALL AXEFND (4, 'FLAG', CATBLK(KIPCN), CATH(KHPTP), FGOFF,
     *   JERR)
      IF (JERR .EQ. 0) THEN
         PIPELN = .TRUE.
         IERR = 0
         END IF
C                                       Add source r.p., if data
C                                       have 'FLAG' r.p., then
C                                       overwrite it with the
C                                       source r.p.
      IF (.NOT.PIPELN) CATBLK(KIPCN) = CATBLK(KIPCN) + 1
      INDEX = KHPTP + (CATBLK(KIPCN)-1)*2
      IF (PIPELN) INDEX = KHPTP + FGOFF*2
      CALL CHR2H (8, 'SOURCE  ', 1, CATH(INDEX))
      CALL AXEFND (8, 'SOURCE  ', CATBLK(KIPCN), CATH(KHPTP), SRCOFF,
     *   JERR)
C                                       Get new uv header info.
      IF (PIPELN) THEN
         CALL UVPGET (IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Add FRQSEL random parm
C                                       if FQ table exists
      ADDFQ = .FALSE.
      CALL AXEFND (7, 'FREQSEL ', CATBLK(KIPCN), CATH(KHPTP), FQOFF,
     *   JERR)
      IF (JERR.EQ.1) THEN
         CALL FNDEXT ('FQ', CATBLK, NUMFQ)
         IF (NUMFQ.GT.0) THEN
            ADDFQ = .TRUE.
            CATBLK(KIPCN) = CATBLK(KIPCN) + 1
            INDEX = KHPTP + (CATBLK(KIPCN)-1)*2
            CALL CHR2H (8, 'FREQSEL ', 1, CATH(INDEX))
            CALL AXEFND (7, 'FREQSEL ', CATBLK(KIPCN), CATH(KHPTP),
     *         FQOFF, JERR)
            END IF
         END IF
C                                       Get position of WEIGHT and
C                                       SCALE r.p's, if they exist.
      CMPRES = CATBLK(KINAX).EQ.1
      IF (CMPRES) THEN
C                                       Look for Weight and scale
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *      WTOFF, IERR)
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP),
     *      SCLOFF, IERR)
         END IF
C                                       At this point SRCOFF, FQOFF,
C                                       WTOFF and SCLOFF represent the
C                                       positions of the various r.p's
C                                       of interest in the output
C                                       header.
C                                       IF WTOFF & SCLOFF are zero
C                                       then we carry on as normal.
      NSHUFL = 0
      RPSHUF = .TRUE.
      IF ((WTOFF.EQ.0) .AND. (SCLOFF).EQ.0) RPSHUF = .FALSE.
      IF (.NOT.RPSHUF) GO TO 999
C                                       If WTOFF & SCLOFF are > the
C                                       other r.p's we carry on. Only
C                                       need to check WTOFF because
C                                       it is always followed
C                                       immediately by SCLOFF.
      IF ((WTOFF.GT.SRCOFF) .AND. (WTOFF.GT.FQOFF)) RPSHUF = .FALSE.
      IF (.NOT.RPSHUF) GO TO 999
C                                       In this case we need to
C                                       rearrange the r.p's so
C                                       WEIGHT and SCALE are the last.
      DO 10 I = 1, CATBLK(KIPCN)
         INDEX = KHPTP + 2 * (I-1)
         CALL H2CHR (8, 1, CATH(INDEX), RPCHR(I))
         RPLOC(I) = I - 1
 10      CONTINUE
C                                       How many r.p.'s exist after
C                                       SCALE
      SCPOS = SCLOFF + 1
      NREPOS = CATBLK(KIPCN) - SCPOS
      NSHUFL = NREPOS + 2
C                                       Reposition them in the header
C                                       and form the look up table
C                                       for the on-the-fly repositioning
      J = 0
      WTPOS = WTOFF + 1
      DO 20 I = WTPOS, CATBLK(KIPCN)
         J = J + 1
         TMPCHR(J) = RPCHR(I)
 20      CONTINUE
      J = 0
      NT = CATBLK(KIPCN) - 2
      OLDLOC(1) = WTOFF
      OLDLOC(2) = SCLOFF
      DO 30 I = WTPOS, NT
         J = J + 1
         RPCHR(I) = TMPCHR(J+2)
         OLDLOC(J+2) = I+1
         NEWLOC(J+2) = I-1
 30      CONTINUE
      RPCHR(NT+1) = TMPCHR(1)
      RPCHR(NT+2) = TMPCHR(2)
      NEWLOC(1) = CATBLK(KIPCN) - 2
      NEWLOC(2) = CATBLK(KIPCN) - 1
C                                       Rewrite the headers
      DO 40 I = 1, CATBLK(KIPCN)
         INDEX = KHPTP + 2 * (I-1)
         CALL CHR2H (8, RPCHR(I), 1, CATH(INDEX))
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE SHUFFL (RPARM, NRP, OLDLOC, NEWLOC, NSHUFL)
C-----------------------------------------------------------------------
C  Routine to perform the shuffling of the random parameters we set up
C  in RPCHK.
C  Inputs/Output:
C    RPARM       R(*)      Array containing random parameters, on
C                          output will contain the shuffled ones.
C  Input:
C    NRP         I         Number of r.p's
C    OLDLOC      I(*)      Old location of r.p's to be shuffled
C    NEWLOC      I(*)      New location of r.p's
C    NSHUFL      I         Number to be shuffled
C-----------------------------------------------------------------------
      REAL    RPARM(*)
      INTEGER NRP, OLDLOC(*), NEWLOC(*), NSHUFL
C
      REAL    TPARM(20)
      INTEGER I
C-----------------------------------------------------------------------
C                                         Copy to temporary array
      CALL RCOPY (NRP, RPARM, TPARM)
C                                         Shuffle them around
      DO 10 I = 1, NSHUFL
         RPARM(NEWLOC(I)+1) = TPARM(OLDLOC(I)+1)
 10      CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
      END
