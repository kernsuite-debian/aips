      PROGRAM UNCAL
C-----------------------------------------------------------------------
C! Task to copy 'uncal' table from the AT to 'cal' tables
C# EXT-util Utility Calibration AT
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2015
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
C  UNCAL copies either CU or BU tables to specified CL and BP
C  tables in order to be able to remove the calibration applied
C  on-line to AT data.
C   ADVERBS:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INEXT       Input table extension type
C    INVERS      Input table file version no.
C-----------------------------------------------------------------------
      CHARACTER  NAMEIN*12, CLAIN*6, FTYPE*2, STAT*4, XEXT*2, OEXT*2,
     *   PRGM*6, ATIME*8, ADATE*12
      HOLLERITH XNAMIN(3), XCLAIN(2), XNEXT
      INTEGER  IRET, SEQIN, DISKIN, OLDCNO, BUFF1(256), IVER, OVER,
     *   LUN1, LUN2, NPARM, IROUND, TIME(3), DATE(3)
      REAL      XSEQIN, XDISKI, XIVER
      LOGICAL   T
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XNEXT, XIVER
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'UNCAL '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 9
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IRET.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XNEXT, XEXT)
      IF ((XEXT.EQ.'CU') .OR. (XEXT.EQ.'  ')) THEN
         OEXT = 'CL'
      ELSE IF (XEXT.EQ.'BU') THEN
         OEXT = 'BP'
      ELSE
         WRITE (MSGTXT,1010) XEXT
         IRET = 1
         GO TO 990
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
      IVER = IROUND (XIVER)
      IF (IVER.LT.1) IVER = 1
      OVER = 0
      XIVER = IVER
C                                       Find input
      OLDCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK and mark 'WRIT'
      STAT = 'WRIT'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, STAT, BUFF1, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Copy the tables
      IF (OEXT.EQ.'CL') THEN
         CALL CUTAB (DISKIN, OLDCNO, IVER, OVER, CATBLK, LUN2, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1050) IRET
            GO TO 990
            END IF
      ELSE IF (OEXT.EQ.'BP') THEN
         CALL BUTAB (DISKIN, OLDCNO, IVER, OVER, CATBLK, LUN2, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1060) IRET
            GO TO 990
            END IF
         END IF
C                                       Finished copying tables
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKIN, OLDCNO, BUFF1, IRET)
      IF (IRET.GT.2) THEN
         WRITE (MSGTXT,1090) IRET
         CALL MSGWRT (6)
         GO TO 150
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (MSGTXT,1100) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, MSGTXT, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 150
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IRET)
      IF (IRET.NE.0) GO TO 150
C                                       Type and version
      WRITE (MSGTXT,2000) TSKNAM, XEXT, IVER, OEXT, OVER
      CALL HIADD (LUN1, MSGTXT, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 150
C                                       Close HI file
 150  CALL HICLOS (LUN1, T, BUFF1, IRET)
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1010 FORMAT ('ERROR: UNKNOWN TABLE TYPE ',A2,' CHECK INPUT PARMS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('CUTAB ERROR ',I3)
 1060 FORMAT ('BUTAB ERROR ',I3)
 1090 FORMAT ('ERROR ',I3,' OPENING HISTORY TABLE')
 1100 FORMAT (A6,'Release = ''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' Copied ''',A2,''' INVERS=',I4,' to ''',A2,
     *   ''' OUTVERS=', I4)
      END
      SUBROUTINE CUTAB (DISK, CNO, VER, OVER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C   Routine to copy a CU table to a CL table
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to copy from
C      OVER            I       Version to copy to
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, OVER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), ICLRNO, NUMANT, NUMPOL, NUMIF, OLUN,
     *   OBUFF(512), OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OCLRNO, NTERM, CUKOLS(50),
     *   CUNUMV(50)
      REAL      DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      REAL    CLKGD(2,MAXIF), DCLKGD(2,MAXIF), CLKPD(2,MAXIF),
     *   DCLKPD(2,MAXIF), ATMGD(2,MAXIF), DATMGD(2,MAXIF),
     *   ATMPD(2,MAXIF),  DATMPD(2,MAXIF), TSYS(2,MAXIF)
      DOUBLE PRECISION    GEODLY(3), GEOPHA, GEORAT
      REAL    GMMOD, TIMEI, IFR
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /MAXIF*0.0, 6*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
C-----------------------------------------------------------------------
C                                       Open CU file
      CALL CUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICLRNO, CUKOLS, CUNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NCLROW = BUFFER(5)
C                                       Open up new CL table
      CALL CALINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      WRITE (MSGTXT,1000) VER, OVER
      CALL MSGWRT (6)
C                                       Loop and copy
      DO 100 I = 1, NCLROW
         ICLRNO = I
         CALL TABCU ('READ', BUFFER, ICLRNO, CUKOLS, CUNUMV,
     *      NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *      IFR, GEODLY(1), GEOPHA, GEORAT, DOPOFF, CLKGD, DCLKGD,
     *      CLKPD, DCLKPD, ATMGD, DATMGD, ATMPD, DATMPD, CREAL, CIMAG,
     *      DELAY, RATE, TSYS, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
         OCLRNO = I
         CALL TABCAL ('WRIT', OBUFF, OCLRNO, OKOLS, ONUMV,
     *      NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OCLRNO, OBUFF, OBUFF, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copying CU table #',I3,' to CL table #',I3)
 1010 FORMAT ('CUTAB: ERROR ',I3,' INITING CU TABLE')
 1020 FORMAT ('CUTAB: ERROR ',I3,' INITING CL TABLE')
 1030 FORMAT ('CUTAB: ERROR ',I3,' READING CU TABLE')
 1040 FORMAT ('CUTAB: ERROR ',I3,' WRITING CL TABLE')
      END
      SUBROUTINE BUTAB (DISK, CNO, VER, OVER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C   Routine to copy a BU table to a BP table
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to copy from
C      OVER            I       Version to copy to
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, OVER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LBPKEY*8, LEIGHT*8
      INTEGER BUFFER(512), IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC),
     *   NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN, OLUN, OBUFF(512),
     *   OKOLS(MAXBPC), ONUMV(MAXBPC), NBPROW, I, SOURID, ANTNO, SUBA,
     *   FREQID, REFANT(2), OBPRNO, NUMSHF
      REAL    TIMEI, BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), BANDW, LOWSHF,
     *   DELSHF
      DOUBLE PRECISION TIME, IFFREQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/, LEIGHT /'        '/
C-----------------------------------------------------------------------
C                                       Open BU file
      CALL BUINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, IBPRNO,
     *   BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NBPROW = BUFFER(5)
      LBPKEY = LEIGHT
C                                       Open up new BP table
      CALL BPINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN, OBPRNO,
     *   OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN, NUMSHF,
     *   LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1000) VER, OVER
      CALL MSGWRT (6)
C                                       Loop and copy
      DO 100 I = 1, NBPROW
         IBPRNO = I
         CALL TABBU ('READ', BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, TIME, TIMEI, SOURID, SUBA, ANTNO, BANDW,
     *      IFFREQ, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
         IF (IRET.LT.0) IRET = 0
         OBPRNO = I
         CALL TABBP ('WRIT', OBUFF, OBPRNO, OKOLS, ONUMV, NUMIF, NUMFRQ,
     *      NUMPOL, TIME, TIMEI, SOURID, SUBA, ANTNO, BANDW, IFFREQ,
     *      FREQID, REFANT, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
         IF (IRET.LT.0) IRET = 0
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IBPRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OBPRNO, OBUFF, OBUFF, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Copying BU table #',I3,' to BP table #',I3)
 1010 FORMAT ('BUTAB: ERROR ',I3,' RETURNED FROM BUINI')
 1030 FORMAT ('BUTAB: ERROR ',I3,' READING BU TABLE')
 1040 FORMAT ('BUTAB: ERROR ',I3,' WRITING BP TABLE')
      END
      SUBROUTINE CUINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICLRNO, CUKOLS, CUNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
C-----------------------------------------------------------------------
C   Creates and initializes UNcalibration extension tables. Slightly
C   modified version of CALINI, used to read the CU tables written
C   by the AT.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   CL file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C    Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of IFs per pair
C     NUMIF        I    Number of IF pairs
C     NTERM        I    Number of terms is model polynomial
C     GMMOD        R    Mean gain modulus
C    Output:
C     ICLRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     CUKOLS(MAXCLC) I   The column pointer array in order, TIME,
C                        TIME INT., SOURCE ID., ANTENNA NO., SUBARRAY,
C                        FREQID, IFR (Ionesph. Faraday Rot.),
C                        GEODELAY, GEOPHASE, GEORATE, DOPPOFF,
C                        CLKGD 1, DCLKGD 1, CLKPD 1, DCLKPD 1,
C                        ATMGD 1, DATMGD 1, ATMPD 1, DATPGD 1,
C                        REAL1, IMAG1, RATE 1, DELAY 1, TSYS1, WEIGHT1,
C                        REFANT 1
C                        Following used if 2 polarizations per IF
C                        CLKGD 2, DCLKGD 2, CLKPD 2, DCLKPD 2,
C                        ATMGD 2, DATMGD 2, ATMPD 2, DATPGD 2,
C                        REAL2, IMAG2, RATE 2, DELAY 2, TSYS2, WEIGHT2,
C                        REFANT 2
C     CUNUMV(MAXCLC) I   Element count in each column.
C     IRET         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH HOLTMP(6)
      CHARACTER OPCODE*4, TTITLE*56, TITLE1(15)*24, TITLE2(13)*24,
     *   TITLE3(13)*24, TITLE(41)*24, UNITS(41)*8, KEYW(4)*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IRET,
     *   NKEY, NREC, DATP(128,2), NCOL, CUKOLS(41), NTT,
     *   DTYP(41), NDATA, KLOCS(4), KEYVAL(6), IPOINT,
     *   NUMANT, NUMPOL, NUMIF, NTERM, CUNUMV(41), KEYTYP(4)
      LOGICAL   T, F, DOREAD, NEWFIL
      INTEGER   ICLRNO, I
      REAL      GMMOD, KEYR(6)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TITLE(1), TITLE1), (TITLE(16), TITLE2),
     *   (TITLE(29), TITLE3)
      EQUIVALENCE (KEYVAL, KEYR)
      DATA T, F /.TRUE.,.FALSE./
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE CALIBRATION TABLE        '/
      DATA DTYP /11,12,4*14,12,3*11,2,8*2,6*2,4,8*2,6*2,4/
      DATA TITLE1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'GEOPHASE                ',
     *   'GEORATE                 ', 'DOPPOFF                 ',
     *   'CLKGD 1                 ', 'DCLKGD 1                ',
     *   'CLKPD 1                 ', 'DCLKPD 1                '/
      DATA TITLE2 /'ATMGD 1                 ',
     *   'DATMGD 1                ',
     *   'ATMPD 1                 ', 'DATMPD 1                ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'TSYS 1                  ', 'WEIGHT 1                ',
     *   'REFANT 1                ',
     *   'CLKGD 2                 ', 'DCLKGD 2                '/
      DATA TITLE3 /'CLKPD 2                 ',
     *   'DCLKPD 2                ',
     *   'ATMGD 2                 ', 'DATMGD 2                ',
     *   'ATMPD 2                 ', 'DATMPD 2                ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'TSYS 2                  ', 'WEIGHT 2                ',
     *   'REFANT 2                '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ', 'MGMOD   '/
      DATA UNITS /'DAYS    ','DAYS    ', 4*'        ', 'RAD/M**2',
     *   'SECONDS ', 'TURNS   ', 'HZ      ','HZ      ',
     *   'SECONDS ', 'SEC/SEC ', 'SECONDS ', 'SEC/SEC ',
     *   'SECONDS ', 'SEC/SEC ', 'SECONDS ', 'SEC/SEC ',
     *   2*'        ', 'SEC/SEC ', 'SECONDS ', 'KELVINS ',2*'        ',
     *   'SECONDS ', 'SEC/SEC ', 'SECONDS ', 'SEC/SEC ',
     *   'SECONDS ', 'SEC/SEC ', 'SECONDS ', 'SEC/SEC ',
     *   2*'        ', 'SEC/SEC ', 'SECONDS ', 'KELVINS ',
     *   2*'        '/
C-----------------------------------------------------------------------
C                                       Assume no real polynomial
      NTERM = 1
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 11 + NUMPOL * 15
      IF (DOREAD) NCOL = 0
      NKEY = 4
      NEWFIL = F
      NDATA = MAXCLC
      CALL FILL (NDATA, 0, CUKOLS)
      CALL FILL (NDATA, 0, CUNUMV)
      IF (DOREAD) GO TO 20
C                                       Fill in types
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 I = 11,NDATA
            DATP(I,2) = DTYP(I) + 10 * NUMIF
 10         CONTINUE
C                                       Create/open file
 20   CALL TABINI (OPCODE, 'CL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) GO TO 990
      NKEY = 4
C                                       See if file exists.
      IF (IRET.EQ.0) GO TO 100
      NEWFIL = T
C                                       File created, initialize
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Get number of scans
 100  ICLRNO = BUFFER(5)
C                                       Set ICLRNO
      ICLRNO = ICLRNO + 1
      IF (DOREAD) ICLRNO = 1
C                                       Read/write keywords
C                                       Set keyword values
      IF (DOREAD) GO TO 120
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. IFs per pair.
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IF pairs.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       Gain modulus
         KLOCS(4) = 4
         KEYTYP(4) = 1
         KEYVAD = GMMOD
         CALL RCOPY (NWDPDP, KEYVAD, KEYR(4))
C                                       Only write if just created.
 120  IF (DOREAD.OR.NEWFIL) CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER,
     *   KLOCS, KEYVAL, KEYTYP, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.20)) GO TO 990
C                                       Read keywords
      CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *   IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.20)) GO TO 990
C                                       Retrieve keyword values
C                                       No. antennas.
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Gain modulus
      IPOINT = KLOCS(4)
      IF (IPOINT.GT.0) THEN
         IF (KEYTYP(4).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
            GMMOD = KEYVAD
         ELSE
            GMMOD = KEYR(IPOINT)
            END IF
         END IF
C                                       Get array indices
      DO 150 I = 1,NDATA
         CUKOLS(I) = DATP(I,1)
         CUNUMV(I) = DATP(I,2) / 10
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1150) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('CUINI: ERROR INITIALIZING CU TABLE FOR ',A4)
      END
      SUBROUTINE TABCU (OPCODE, BUFFER, ICLRNO, CUKOLS, CUNUMV,
     *   NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *   GEODLY, GEOPHA, GEORAT, DOPOFF, CLKGD, DCLKGD, CLKPD, DCLKPD,
     *   ATMGD, DATMGD, ATMPD, DATMPD, CREAL, CIMAG, DELAY, RATE, TSYS,
     *   WEIGHT, REFA, IRET)
C-----------------------------------------------------------------------
C   Does I/O to UNCALIBRATION extention tables.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by TABINI or
C                      TABINI.
C     ICLRNO       I   Next scan number to read or write.
C     CUKOLS(MAXCLC) I   The column pointer array in order, TIME,
C                        TIME INT., SOURCE ID., ANTENNA NO., SUBARRAY,
C                        FREQID, IFR (Ionesph. Faraday Rot.),
C                        GEODELAY, GEOPHASE, GEORATE, DOPPOFF,
C                        CLKGD 1, DCLKGD 1, CLKPD 1, DCLKPD 1,
C                        ATMGD 1, DATMGD 1, ATMPD 1, DATPGD 1,
C                        REAL1, IMAG1, RATE 1, DELAY 1, TSYS1, WEIGHT1,
C                        REFANT 1
C                        Following used if 2 polarizations per IF
C                        CLKGD 2, DCLKGD 2, CLKPD 2, DCLKPD 2,
C                        ATMGD 2, DATMGD 2, ATMPD 2, DATPGD 2,
C                        REAL2, IMAG2, RATE 2, DELAY 2, TSYS2, WEIGHT2,
C                        REFANT 2
C     CUNUMV(MAXCLC) I   Element count in each column.
C     NUMPOL       I   Number of polarizations per IF.
C     NUMIF        I   Number of IFs.
C    Input/output: (written to or read from CAL file)
C     TIME         D   Center time of CAL record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     ANTNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     FREQID       I   Freqid #
C     IFR          R   Ionospheric Faraday Rotation (rad/m**2)
C     GEODLY       D   Geometric delay at TIME (sec)
C     GEOPHA       D   Phase of sinusoid (turns)
C     GEORAT       D   Time rate of change of GEOPHA (Hz)
C     DOPOFF(*)    R   Doppler offset for each IF (Hz)
C     CLKGD(2,*)   R   "Clock" Group delay (sec) 1/poln/IF
C     DCLKGD(2,*)  R   Time derivative of "Clock" Group delay (sec/sec)
C     CLKPD(2,*)   R   "Clock" Phase delay (sec) 1/poln/IF
C     DCLKPD(2,*)  R   Time derivative of "Clock" Phase delay (sec/sec)
C     ATMGD(2,*)   R   "Atmos" Group delay (sec) 1/poln/IF
C     DATMGD(2,*)  R   Time derivative of "Atmos" Group delay (sec/sec)
C     ATMPD(2,*)   R   "Atmos" Phase delay (sec) 1/poln/IF
C     DATMPD(2,*)  R   Time derivative of "Atmos" Phase delay (sec/sec)
C     CREAL(2,*)   R   Real part of the complex gain, 1/poln/IF
C     CIMAG(2,*)   R   Imag part of the complex gain, 1/poln/IF
C     DELAY(2,*)   R   Residual group delay (sec), 1/poln/IF
C     RATE(2,*)    R   Residual fringe rate (Hz), 1/poln/IF
C     TSYS(2,*)    R   System temperature (K), 1/poln/IF
C     WEIGHT(2,*)  R   Weight of solution, 1/poln/IF
C     REFA(2,*)    I   Reference antenna use for cal. solution.
C    Output:
C     ICLRNO       I   Next CAL number.
C     IRET         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), CUKOLS(*), CUNUMV(*), NUMPOL,
     *   NUMIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,MAXIF), IRET,
     *   RECI(1), POINT, LOOP, TYPE, IPT,
     *   DOPKOL, CGDKOL, DCGKOL, CPDKOL, DCPKOL, AGDKOL, DAGKOL,
     *   APDKOL, DAPKOL, REAKOL, IMAKOL, DELKOL, RATKOL, TSYKOL,
     *   WEIKOL, REFKOL, ICLRNO
      LOGICAL   DOREAD, DOCLOS
      REAL      TIMEI, IFR, DOPOFF(MAXIF), CLKGD(2,MAXIF),
     *   DCLKGD(2,MAXIF), CLKPD(2,MAXIF), DCLKPD(2,MAXIF),
     *   ATMGD(2,MAXIF), DATMGD(2,MAXIF), ATMPD(2,MAXIF),
     *   DATMPD(2,MAXIF), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), TSYS(2,MAXIF), WEIGHT(2,MAXIF),
     *   RECORD(11+32*MAXIF)
      DOUBLE PRECISION TIME, GEODLY, GEOPHA, GEORAT, RECD(1)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      DOCLOS = OPCODE.EQ.'CLOS'
C                                       Branch for CLOS
      IF (DOCLOS) GO TO 800
C                                       If write fill RECORD
      IF (DOREAD) GO TO 50
         POINT = CUKOLS(1)
         RECD(POINT) = TIME
         POINT = CUKOLS(2)
         RECORD(POINT) = TIMEI
         POINT = CUKOLS(3)
         RECI(POINT) = SOURID
         POINT = CUKOLS(4)
         RECI(POINT) = ANTNO
         POINT = CUKOLS(5)
         RECI(POINT) = SUBA
         POINT = CUKOLS(6)
         RECI(POINT) = FREQID
         POINT = CUKOLS(7)
         RECORD(POINT) = IFR
         POINT = CUKOLS(8)
         RECD(POINT) = GEODLY
         POINT = CUKOLS(9)
         RECD(POINT) = GEOPHA
         POINT = CUKOLS(10)
         RECD(POINT) = GEORAT
C                                       Setup for looping over IF
         DOPKOL = CUKOLS(11)
         CGDKOL = CUKOLS(12)
         DCGKOL = CUKOLS(13)
         CPDKOL = CUKOLS(14)
         DCPKOL = CUKOLS(15)
         AGDKOL = CUKOLS(16)
         DAGKOL = CUKOLS(17)
         APDKOL = CUKOLS(18)
         DAPKOL = CUKOLS(19)
         REAKOL = CUKOLS(20)
         IMAKOL = CUKOLS(21)
         RATKOL = CUKOLS(22)
         DELKOL = CUKOLS(23)
         TSYKOL = CUKOLS(24)
         WEIKOL = CUKOLS(25)
         REFKOL = CUKOLS(26)
C                                       First polarization
         DO 10 LOOP = 1,NUMIF
            RECORD(DOPKOL) = DOPOFF(LOOP)
            RECORD(CGDKOL) = CLKGD(1,LOOP)
            RECORD(DCGKOL) = DCLKGD(1,LOOP)
            RECORD(CPDKOL) = CLKPD(1,LOOP)
            RECORD(DCPKOL) = DCLKPD(1,LOOP)
            RECORD(AGDKOL) = ATMGD(1,LOOP)
            RECORD(DAGKOL) = DATMGD(1,LOOP)
            RECORD(APDKOL) = ATMPD(1,LOOP)
            RECORD(DAPKOL) = DATMPD(1,LOOP)
            RECORD(REAKOL) = CREAL(1,LOOP)
            RECORD(IMAKOL) = CIMAG(1,LOOP)
            RECORD(DELKOL) = DELAY(1,LOOP)
            RECORD(RATKOL) = RATE(1,LOOP)
            RECORD(TSYKOL) = TSYS(1,LOOP)
            RECORD(WEIKOL) = WEIGHT(1,LOOP)
            RECI(REFKOL) = REFA(1,LOOP)
            IF (LOOP.GE.NUMIF) GO TO 10
               DOPKOL = DOPKOL+1
               CGDKOL = CGDKOL+1
               DCGKOL = DCGKOL+1
               CPDKOL = CPDKOL+1
               DCPKOL = DCPKOL+1
               AGDKOL = AGDKOL+1
               DAGKOL = DAGKOL+1
               APDKOL = APDKOL+1
               DAPKOL = DAPKOL+1
               REAKOL = REAKOL+1
               IMAKOL = IMAKOL+1
               DELKOL = DELKOL+1
               RATKOL = RATKOL+1
               TSYKOL = TSYKOL+1
               WEIKOL = WEIKOL+1
               REFKOL = REFKOL+1
 10         CONTINUE
         IF (NUMPOL.LT.2) GO TO 50
C                                       Two polarizations
C                                       Setup for looping over IF
            CGDKOL = CUKOLS(27)
            DCGKOL = CUKOLS(28)
            CPDKOL = CUKOLS(29)
            DCPKOL = CUKOLS(30)
            AGDKOL = CUKOLS(31)
            DAGKOL = CUKOLS(32)
            APDKOL = CUKOLS(33)
            DAPKOL = CUKOLS(34)
            REAKOL = CUKOLS(35)
            IMAKOL = CUKOLS(36)
            RATKOL = CUKOLS(37)
            DELKOL = CUKOLS(38)
            TSYKOL = CUKOLS(39)
            WEIKOL = CUKOLS(40)
            REFKOL = CUKOLS(41)
            DO 20 LOOP = 1,NUMIF
               RECORD(CGDKOL) = CLKGD(2,LOOP)
               RECORD(DCGKOL) = DCLKGD(2,LOOP)
               RECORD(CPDKOL) = CLKPD(2,LOOP)
               RECORD(DCPKOL) = DCLKPD(2,LOOP)
               RECORD(AGDKOL) = ATMGD(2,LOOP)
               RECORD(DAGKOL) = DATMGD(2,LOOP)
               RECORD(APDKOL) = ATMPD(2,LOOP)
               RECORD(DAPKOL) = DATMPD(2,LOOP)
               RECORD(REAKOL) = CREAL(2,LOOP)
               RECORD(IMAKOL) = CIMAG(2,LOOP)
               RECORD(DELKOL) = DELAY(2,LOOP)
               RECORD(RATKOL) = RATE(2,LOOP)
               RECORD(TSYKOL) = TSYS(2,LOOP)
               RECORD(WEIKOL) = WEIGHT(2,LOOP)
               RECI(REFKOL) = REFA(2,LOOP)
               IF (LOOP.GE.NUMIF) GO TO 20
                  CGDKOL = CGDKOL+1
                  DCGKOL = DCGKOL+1
                  CPDKOL = CPDKOL+1
                  DCPKOL = DCPKOL+1
                  AGDKOL = AGDKOL+1
                  DAGKOL = DAGKOL+1
                  APDKOL = APDKOL+1
                  DAPKOL = DAPKOL+1
                  REAKOL = REAKOL+1
                  IMAKOL = IMAKOL+1
                  DELKOL = DELKOL+1
                  RATKOL = RATKOL+1
                  TSYKOL = TSYKOL+1
                  WEIKOL = WEIKOL+1
                  REFKOL = REFKOL+1
 20               CONTINUE
C                                       Process record.
 50   CALL TABIO (OPCODE, 0, ICLRNO, RECORD, BUFFER, IRET)
      ICLRNO = ICLRNO + 1
      IF (IRET.GT.0) GO TO 990
      IF (IRET.LT.0) GO TO 50
C                                       If READ pick data from RECORD.
      IF (.NOT.DOREAD) GO TO 999
C                                       Determine how many columns we
C                                       have
         TYPE = 1
         IF ((BUFFER(10).EQ.41) .OR. (BUFFER(10).EQ.26)) TYPE = 2
         IPT = 1
         POINT = CUKOLS(IPT)
         TIME = RECD(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         TIMEI = RECORD(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         SOURID = RECI(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         ANTNO = RECI(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         SUBA = RECI(POINT)
         IF (TYPE.EQ.2) THEN
            IPT = IPT + 1
            POINT = CUKOLS(IPT)
            FREQID = RECI(POINT)
            IPT = IPT + 1
            POINT = CUKOLS(IPT)
            IFR = RECORD(POINT)
            END IF
         IF (TYPE.EQ.1) THEN
            FREQID = 1
            IFR = 0.0
            END IF
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         GEODLY = RECD(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         GEOPHA = RECD(POINT)
         IPT = IPT + 1
         POINT = CUKOLS(IPT)
         GEORAT = RECD(POINT)
C                                       Setup for looping over IF
         IPT = IPT + 1
         DOPKOL = CUKOLS(IPT)
         CGDKOL = CUKOLS(IPT+1)
         DCGKOL = CUKOLS(IPT+2)
         CPDKOL = CUKOLS(IPT+3)
         DCPKOL = CUKOLS(IPT+4)
         AGDKOL = CUKOLS(IPT+5)
         DAGKOL = CUKOLS(IPT+6)
         APDKOL = CUKOLS(IPT+7)
         DAPKOL = CUKOLS(IPT+8)
         REAKOL = CUKOLS(IPT+9)
         IMAKOL = CUKOLS(IPT+10)
         RATKOL = CUKOLS(IPT+11)
         DELKOL = CUKOLS(IPT+12)
         TSYKOL = CUKOLS(IPT+13)
         WEIKOL = CUKOLS(IPT+14)
         REFKOL = CUKOLS(IPT+15)
C                                       First polarization
         DO 60 LOOP = 1,NUMIF
            DOPOFF(LOOP) = RECORD(DOPKOL)
            CLKGD(1,LOOP) = RECORD(CGDKOL)
            DCLKGD(1,LOOP) = RECORD(DCGKOL)
            CLKPD(1,LOOP) = RECORD(CPDKOL)
            DCLKPD(1,LOOP) = RECORD(DCPKOL)
            ATMGD(1,LOOP) = RECORD(AGDKOL)
            DATMGD(1,LOOP) = RECORD(DAGKOL)
            ATMPD(1,LOOP) = RECORD(APDKOL)
            DATMPD(1,LOOP) = RECORD(DAPKOL)
            CREAL(1,LOOP) = RECORD(REAKOL)
            CIMAG(1,LOOP) = RECORD(IMAKOL)
            DELAY(1,LOOP) = RECORD(DELKOL)
            RATE(1,LOOP) = RECORD(RATKOL)
            TSYS(1,LOOP) = RECORD(TSYKOL)
            WEIGHT(1,LOOP) = RECORD(WEIKOL)
            REFA(1,LOOP) = RECI(REFKOL)
            IF (LOOP.GE.NUMIF) GO TO 60
               DOPKOL = DOPKOL+1
               CGDKOL = CGDKOL+1
               DCGKOL = DCGKOL+1
               CPDKOL = CPDKOL+1
               DCPKOL = DCPKOL+1
               AGDKOL = AGDKOL+1
               DAGKOL = DAGKOL+1
               APDKOL = APDKOL+1
               DAPKOL = DAPKOL+1
               REAKOL = REAKOL+1
               IMAKOL = IMAKOL+1
               DELKOL = DELKOL+1
               RATKOL = RATKOL+1
               TSYKOL = TSYKOL+1
               WEIKOL = WEIKOL+1
               REFKOL = REFKOL+1
 60         CONTINUE
         IF (NUMPOL.LT.2) GO TO 999
C                                       Two polarizations
C                                       Setup for looping over IF
            CGDKOL = CUKOLS(IPT+16)
            DCGKOL = CUKOLS(IPT+17)
            CPDKOL = CUKOLS(IPT+18)
            DCPKOL = CUKOLS(IPT+19)
            AGDKOL = CUKOLS(IPT+20)
            DAGKOL = CUKOLS(IPT+21)
            APDKOL = CUKOLS(IPT+22)
            DAPKOL = CUKOLS(IPT+23)
            REAKOL = CUKOLS(IPT+24)
            IMAKOL = CUKOLS(IPT+25)
            RATKOL = CUKOLS(IPT+26)
            DELKOL = CUKOLS(IPT+27)
            TSYKOL = CUKOLS(IPT+28)
            WEIKOL = CUKOLS(IPT+29)
            REFKOL = CUKOLS(IPT+30)
            DO 70 LOOP = 1,NUMIF
               CLKGD(2,LOOP) = RECORD(CGDKOL)
               DCLKGD(2,LOOP) = RECORD(DCGKOL)
               CLKPD(2,LOOP) = RECORD(CPDKOL)
               DCLKPD(2,LOOP) = RECORD(DCPKOL)
               ATMGD(2,LOOP) = RECORD(AGDKOL)
               DATMGD(2,LOOP) = RECORD(DAGKOL)
               ATMPD(2,LOOP) = RECORD(APDKOL)
               DATMPD(2,LOOP) = RECORD(DAPKOL)
               CREAL(2,LOOP) = RECORD(REAKOL)
               CIMAG(2,LOOP) = RECORD(IMAKOL)
               DELAY(2,LOOP) = RECORD(DELKOL)
               RATE(2,LOOP) = RECORD(RATKOL)
               TSYS(2,LOOP) = RECORD(TSYKOL)
               WEIGHT(2,LOOP) = RECORD(WEIKOL)
               REFA(2,LOOP) = RECI(REFKOL)
               IF (LOOP.GE.NUMIF) GO TO 70
                  CGDKOL = CGDKOL+1
                  DCGKOL = DCGKOL+1
                  CPDKOL = CPDKOL+1
                  DCPKOL = DCPKOL+1
                  AGDKOL = AGDKOL+1
                  DAGKOL = DAGKOL+1
                  APDKOL = APDKOL+1
                  DAPKOL = DAPKOL+1
                  REAKOL = REAKOL+1
                  IMAKOL = IMAKOL+1
                  DELKOL = DELKOL+1
                  RATKOL = RATKOL+1
                  TSYKOL = TSYKOL+1
                  WEIKOL = WEIKOL+1
                  REFKOL = REFKOL+1
 70               CONTINUE
            GO TO 999
C                                       Close
 800  CALL TABIO ('CLOS', 0, ICLRNO, RECORD, BUFFER, IRET)
      IF (IRET.GT.0) GO TO 990
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABCU: TABIO ERROR',I3)
      END
      SUBROUTINE BUINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *   IRET)
C-----------------------------------------------------------------------
C   Creates and initializes UNbandpass (BU) extension tables. Slightly
C   modified from BPINI for AT data.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   BL file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of polarizations.
C     NUMIF        I    Number of IFs
C     NUMFRQ       I    Number of frequency channels
C     BCHAN        I    Start channel number
C   Output:
C     BUFFER   I(512)   I/O buffer and related storage, also defines
C                       file if open.
C     IBPRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     BPKOLS(MAXBPC) I   The column pointer array in order:
C                        TIME, INTERVAL, SOURID,
C                        SUBARRAY, ANTENNA,
C                        BANDW (of individual channel), IFFREQ,
C                        FREQ. ID,
C                        REFANT1, REAL1, IMAG1,
C                        Following used if 2 polarizations per IF
C                        REFANT2, REAL2, IMAG2.
C     BPNUMV(MAXBPC) I   Element count in each column.
C     IRET         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH HOLTMP(6)
      CHARACTER OPCODE*4, TTITLE*56, TITLE(MAXBPC)*24, UNITS(MAXBPC)*8,
     *   KEYW(5)*8, ATITLE(MAXBPC)*24
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IRET, J,
     *   NKEY, NREC, DATP(128,2), NCOL, BPKOLS(MAXBPC), BPNUMV(MAXBPC),
     *   NTT, DTYP(MAXBPC), NDATA, KLOCS(5), KEYVAL(5), KEYTYP(5),
     *   IPOINT, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN
      LOGICAL   T, F, DOREAD, NEWFIL
      INTEGER   IBPRNO, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE BANDPASS TABLE            '/
      DATA DTYP /11, 12, 3*14, 12, 1, 14, 14, 3*2, 14, 3*2/
      DATA TITLE /'TIME ', 'INTERVAL ', 'SOURCE ID ', 'SUBARRAY ',
     *   'ANTENNA ', 'BANDWIDTH ', 'IF FREQ ', 'FREQ ID ',
     *   'REFANT 1 ', 'WEIGHT 1', 'REAL 1 ', 'IMAG 1 ',
     *   'REFANT 2 ', 'WEIGHT 2', 'REAL 2 ', 'IMAG 2 '/
      DATA KEYW /'NO_ANT', 'NO_POL', 'NO_IF', 'NO_CHAN', 'STRT_CHN'/
      DATA UNITS /'DAYS ', 'DAYS ',3*' ','HZ ','HZ ',9*' '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NCOL = 8 + NUMPOL * 4
      IF (DOREAD) NCOL = 0
      NKEY = 5
      NEWFIL = F
      NDATA = MAXBPC
      CALL FILL (NDATA, 0, BPKOLS)
      CALL FILL (NDATA, 0, BPNUMV)
C                                       Fill in types
      CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       IF freq. type
      DATP(7,2) = DTYP(7) + 10*NUMIF
C                                       Correct for real/imag
      DATP(10,2) = DTYP(10) + 10*NUMIF
      DATP(11,2) = DTYP(11) + 10*NUMFRQ*NUMIF
      DATP(12,2) = DTYP(12) + 10*NUMFRQ*NUMIF
      DATP(14,2) = DTYP(14) + 10*NUMIF
      DATP(15,2) = DTYP(15) + 10*NUMFRQ*NUMIF
      DATP(16,2) = DTYP(16) + 10*NUMFRQ*NUMIF
C                                       Create/open file
      CALL TABINI (OPCODE, 'BP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) GO TO 990
      NKEY = 5
C                                       See if file exists.
      IF (IRET.EQ.0) GO TO 100
      NEWFIL = T
C                                       File created, initialize
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Get number of scans
 100  IBPRNO = BUFFER(5)
C                                       Set IBPRNO
      IBPRNO = IBPRNO + 1
      IF (DOREAD) IBPRNO = 1
C                                       Read/write keywords
C                                       Set keyword values
      IF (.NOT.DOREAD) THEN
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. Polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. CHAN
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NUMFRQ
C                                       Start CHAN
         KLOCS(5) = 5
         KEYTYP(5) = 4
         KEYVAL(5) = BCHAN
         END IF
C                                       Only write if just created.
      IF (NEWFIL) THEN
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) GO TO 990
         END IF
C                                       Read keywords
      CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *   IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.20)) GO TO 990
C                                       Retrieve keyword values
C                                       No. antennas.
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. polarizations
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IFs
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. CHAN
      IPOINT = KLOCS(4)
      IF (IPOINT.GT.0) NUMFRQ = KEYVAL(IPOINT)
C                                       Start chan
      IPOINT = KLOCS(5)
      IF (IPOINT.GT.0) BCHAN = KEYVAL(IPOINT)
C                                       Get array indices
      IF ((NEWFIL) .OR. (NCOL.EQ.NDATA)) THEN
         DO 120 I = 1,NDATA
            BPKOLS(I) = DATP(I,1)
            BPNUMV(I) = DATP(I,2) / 10
 120        CONTINUE
C                                       some are missing
      ELSE
C                                       read column labels
         DO 130 J = 1,NCOL
            CALL TABIO ('READ', 3, J, HOLTMP, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL H2CHR (24, 1, HOLTMP, ATITLE(J))
 130        CONTINUE
         DO 150 I = 1,NDATA
            DO 140 J = 1,NCOL
               IF (ATITLE(J).EQ.TITLE(I)) THEN
                  BPKOLS(I) = DATP(J,1)
                  BPNUMV(I) = DATP(J,2) / 10
                  GO TO 150
                  END IF
 140           CONTINUE
 150        CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1150) OPCODE
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('BUINI: ERROR INITIALIZING BANDPASS TABLE FOR ',A)
      END
      SUBROUTINE TABBU (OPCODE, BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *   NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW, IFFREQ,
     *   FREQID, REFANT, WEIGHT, BNDPAS, IRET)
C-----------------------------------------------------------------------
C   Does I/O to bandpass (BU) extention tables. Usually used after
C   setup by BUINI.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     IBPRNO       I   Next entry number to read or write.
C     BPKOLS(MAXBPC) I   The column pointer array in order,
C                        TIME, INTERVAL, SOURID,
C                        SUBARRAY, ANTENNA,
C                        BANDW (of individual channel), IFREQ, FREQID,
C                        REFANT1, REAL1, IMAG1, .. etc for all channels
C                        Following used if 2 polarizations per IF
C                        REFANT2, REAL2, IMAG2.
C     BPNUMV(MAXBPC) I   Element count in each column.
C     NUMIF        I   Number of IF's
C     NUMFRQ       I   Number of chns
C     NUMPOL       I   Number of polarizations per IF.
C    Input/output: (written to or read from baseline file)
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open. Should have been returned by BPINI or
C                      TABINI.
C     TIME         D   Center time of record (Days)
C     INTERV       R   Time interval of record (Days)
C     SOURID       I   Source ID number.
C     SUBA         I   Subarray number.
C     ANT          I   Antenna number.
C     BANDW        R   Bandwidth of an individual channel (Hz)
C     IFFREQ(m)    D   Reference frequency for each IF (Hz)
C     FREQID       I   Freq. id number
C     REFANT(2)    I   Reference Antenna; one for each poln
C     WEIGHT(m,2)  R   Weights (IFs, polns)
C     BNDPAS(n,m,2) C  Complex bandpass: n channels; m IFS; 2 polns
C    Output:
C     IBPRNO       I   Next solution number.
C     IRET         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but polzn #1 flagged
C                            -2=> read but polzn #2 flagged
C                            -3=> both flagged
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), BPKOLS(MAXBPC), BPNUMV(MAXBPC), NUMPOL, IRET,
     *   SOURID, ANT, SUBA,  RECI(XBPRSZ), KOLS(MAXBPC), NUMIF, NUMFRQ,
     *   TIMKOL, SOUKOL, SUBKOL, ANTKL, INTKOL, BWKOL, IFKOL, FRQKOL,
     *   REF1KL, REF2KL, RE1KL, IM1KL, RE2KL, IM2KL, REFANT(2), LOOP,
     *   IFLP, IBPRNO, FREQID, NDATA, INDX, NNDX, BDCNT1, BDCNT2,
     *   WT1KL, WT2KL
      REAL      INTERV, RECORD(XBPRSZ), BNDPAS(2,*), WEIGHT(*), BANDW,
     *   WT
      DOUBLE PRECISION TIME, IFFREQ(MAXIF), RECD(XBPRSZ/2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), INTKOL),
     *   (KOLS(3), SOUKOL), (KOLS(4), SUBKOL), (KOLS(5), ANTKL),
     *   (KOLS(6), BWKOL),  (KOLS(7), IFKOL), (KOLS(8), FRQKOL),
     *   (KOLS(9), REF1KL), (KOLS(10), WT1KL),  (KOLS(11), RE1KL),
     *   (KOLS(12), IM1KL), (KOLS(13), REF2KL), (KOLS(14), WT2KL),
     *   (KOLS(15),RE2KL), (KOLS(16),IM2KL)
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
      BDCNT1 = 0
      BDCNT2 = 0
      NDATA = MAXBPC
C                                       Branch for CLOS
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IBPRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 990
         GO TO 999
         END IF
C                                       Set pointers
      CALL COPY (NDATA, BPKOLS, KOLS)
C                                       If write fill RECORD
      NNDX = NUMFRQ * NUMIF
      IF (OPCODE.NE.'READ') THEN
         RECD(TIMKOL) = TIME
         RECORD(INTKOL) = INTERV
         RECI(SOUKOL) = SOURID
         RECI(SUBKOL) = SUBA
         RECI(ANTKL) = ANT
         RECORD(BWKOL) = BANDW
         DO 10 IFLP = 1,NUMIF
            RECD(IFKOL) = IFFREQ(IFLP)
            IFKOL = IFKOL + 1
            IF (WT1KL.GT.0) THEN
               RECORD(WT1KL) = WEIGHT(IFLP)
               WT1KL = WT1KL + 1
               END IF
            IF (NUMPOL.GT.1) THEN
               IF (WT2KL.GT.0) THEN
                  RECORD(WT2KL) = WEIGHT(IFLP+NUMIF)
                  WT2KL = WT2KL + 1
                  END IF
               END IF
 10         CONTINUE
         IF (FRQKOL.GT.0) RECI(FRQKOL) = FREQID
         RECI(REF1KL) = REFANT(1)
C                                       First polarization
         INDX = 0
         DO 20 LOOP = 1,NNDX
            INDX = INDX + 1
            RECORD(RE1KL) = BNDPAS(1,INDX)
            RECORD(IM1KL) = BNDPAS(2,INDX)
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
 20         CONTINUE
         IF (NUMPOL.GT.1) THEN
            RECI(REF2KL) = REFANT(2)
            DO 30 LOOP = 1,NNDX
               INDX = INDX + 1
               RECORD(RE2KL) = BNDPAS(1,INDX)
               RECORD(IM2KL) = BNDPAS(2,INDX)
               RE2KL = RE2KL + 1
               IM2KL = IM2KL + 1
 30            CONTINUE
            END IF
         END IF
C                                       Process record.
 60   CALL TABIO (OPCODE, 0, IBPRNO, RECORD, BUFFER, IRET)
      IBPRNO = IBPRNO + 1
      IF (IRET.GT.0) GO TO 990
      IF (IRET.LT.0) GO TO 60
C                                       If READ pick data from RECORD.
      IF (OPCODE.EQ.'READ') THEN
         TIME      = RECD(TIMKOL)
         INTERV   = RECORD(INTKOL)
         SOURID    = RECI(SOUKOL)
         SUBA      = RECI(SUBKOL)
         ANT       = RECI(ANTKL)
         BANDW     = RECORD(BWKOL)
         WT = INTERV * 24. * 60. * 6.
         DO 70 IFLP = 1,NUMIF
            IFFREQ(IFLP) = RECD(IFKOL)
            IFKOL = IFKOL + 1
            IF (WT1KL.GT.0) THEN
               WEIGHT(IFLP) = RECORD(WT1KL)
               WT1KL = WT1KL + 1
            ELSE
               WEIGHT(IFLP) = WT
               END IF
            IF (NUMPOL.GT.1) THEN
               IF (WT2KL.GT.0) THEN
                  WEIGHT(IFLP+NUMIF) = RECORD(WT2KL)
                  WT2KL = WT2KL + 1
               ELSE
                  WEIGHT(IFLP+NUMIF) = WT
                  END IF
               END IF
 70         CONTINUE
         IF (FRQKOL.LE.0) THEN
            FREQID = 1
         ELSE
            FREQID = RECI(FRQKOL)
            END IF
         REFANT(1) = RECI(REF1KL)
C                                       First polarization
         INDX = 0
         DO 80 LOOP = 1,NNDX
            INDX = INDX + 1
            BNDPAS(1,INDX) = RECORD(RE1KL)
            BNDPAS(2,INDX) = RECORD(IM1KL)
            IF ((RECORD(RE1KL).EQ.FBLANK) .OR.
     *         (RECORD(IM1KL).EQ.FBLANK)) BDCNT1 = BDCNT1 + 1
            RE1KL = RE1KL + 1
            IM1KL = IM1KL + 1
 80         CONTINUE
         IF (NUMPOL.GT.1) THEN
            REFANT(2) = RECI(REF2KL)
            DO 90 LOOP = 1,NNDX
               INDX = INDX + 1
               BNDPAS(1,INDX) = RECORD(RE2KL)
               BNDPAS(2,INDX) = RECORD(IM2KL)
               IF ((RECORD(RE2KL).EQ.FBLANK) .OR.
     *             (RECORD(IM2KL).EQ.FBLANK)) BDCNT2 = BDCNT2 + 1
               RE2KL = RE2KL + 1
               IM2KL = IM2KL + 1
 90            CONTINUE
            END IF
         END IF
      IF (BDCNT1.EQ.(NUMIF*NUMFRQ)) IRET = -1
      IF (BDCNT2.EQ.(NUMIF*NUMFRQ)) IRET = -2
      IF ((BDCNT1.EQ.(NUMIF*NUMFRQ)) .AND.
     *   (BDCNT2.EQ.(NUMIF*NUMFRQ))) IRET = -3
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1800) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('TABBU: TABIO ERROR',I3)
      END
