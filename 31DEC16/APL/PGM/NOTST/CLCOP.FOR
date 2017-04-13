      PROGRAM CLCOP
C-----------------------------------------------------------------------
C! Task to copy calibration in SN/CL tables between polarizations
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2005, 2012, 2015
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
C  CLCOP copies AIPS SN/CL extension files, copying one polarization to
C  the other.  The specified output table should not exist before the
C  execution of CLCOP.
C  ADVERBS:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INEXT       Input table extension type
C    INVERS      Input table file version no.
C    OUTNAME     Output image name (name)
C    OUTCLASS    Output image name (class)
C    OUTSEQ      Output image name (seq. #)
C    OUTDISK     Output image disk unit #.
C    OUTVERS     Output table file version.
C    OPTYPE      'R2L', 'L2R', 'SWIT', 'AVER'
C    FPARM       IFs to average
C    VPARM       IFs to be replaced
C-----------------------------------------------------------------------
      HOLLERITH XNAMIN(3), XCLAIN(2), XNEXT, XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, FTYPE*2, OPTYPE*4, PRGM*6, STAT*4,
     *   TABTYP*2, ATIME*8, ADATE*12, HILINE*72
      INTEGER  IRET, SEQIN, DISKIN, CNO, BUFF1(512), BUFF2(512), I,
     *   IERR, IVER, OVER, LUN1, LUN2, NPARM, IROUND, NOLDT, TIME(3),
     *   DATE(3), NA, NB, IA(30), IB(30), I1, I2
      REAL      XSEQIN, XDISKI, XIVER, FPARM(30), VPARM(30)
      LOGICAL   T
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XNEXT, XIVER,
     *   XOPTYP, FPARM, VPARM
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'CLCOP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 70
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XNEXT, TABTYP)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF ((OPTYPE.NE.'R2L') .AND. (OPTYPE.NE.'L2R') .AND.
     *   (OPTYPE.NE.'SWIT') .AND. (OPTYPE.NE.'AVER')) THEN
         MSGTXT = '''' // OPTYPE // ''' UNRECOGNIZED - QUITTING'
         GO TO 990
         END IF
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
      IVER   = IROUND (XIVER)
      IF (TABTYP.NE.'CL') TABTYP = 'SN'
C                                       Find input
      CNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATBLK, NOLDT)
C                                       use highest version if IVER = 0
      IF (IVER.LE.0) IVER = NOLDT
      OVER = NOLDT + 1
C                                       Copy table: IF averaging
      IF (OPTYPE.EQ.'AVER') THEN
         NA = 30
         NB = 30
         DO 10 I = 1,30
            IA(I) = IROUND (FPARM(I))
            IB(I) = IROUND (VPARM(I))
            IF (IA(I).LE.0) NA = MIN (NA, I-1)
            IF (IB(I).LE.0) NB = MIN (NB, I-1)
 10         CONTINUE
         IF ((NA.LE.0) .OR. (NB.LE.0)) THEN
            MSGTXT = 'YOU MUST SPECIFY IFS IN APARM AND BPARM'
            IRET = 10
            GO TO 990
            END IF
         IF (TABTYP.EQ.'CL') THEN
            CALL CLAVR (NA, IA, NB, IB, IVER, OVER, LUN1, LUN2, DISKIN,
     *         CNO, BUFF1, BUFF2, IRET)
         ELSE
            CALL SNAVR (NA, IA, NB, IB, IVER, OVER, LUN1, LUN2, DISKIN,
     *         CNO, BUFF1, BUFF2, IRET)
            END IF
      ELSE
         IF (TABTYP.EQ.'CL') THEN
            CALL CLCOPR (OPTYPE, IVER, OVER, LUN1, LUN2, DISKIN, CNO,
     *         BUFF1, BUFF2, IRET)
         ELSE
            CALL SNCOPR (OPTYPE, IVER, OVER, LUN1, LUN2, DISKIN, CNO,
     *         BUFF1, BUFF2, IRET)
            END IF
         END IF
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'INPUT AND OUTPUT TABLES ARE THE SAME.'
         ELSE IF (IRET.EQ.6) THEN
            MSGTXT = 'OUTPUT TABLE ALREADY EXISTS'
         ELSE
            WRITE (MSGTXT,1085) IRET, IVER
            END IF
         GO TO 990
         END IF
C                                       Finished copying tables
      IRET = 0
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKIN, CNO, BUFF1, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         GO TO 150
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (HILINE,1100) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Type and version
      WRITE (HILINE,2000) TSKNAM, TABTYP, IVER, OVER
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       OPTYPE
      WRITE (HILINE,2001) TSKNAM, OPTYPE
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
      IF (OPTYPE.EQ.'AVER') THEN
         HILINE = TSKNAM // '/  IFs averaged'
         CALL HIADD (LUN1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 150
         I1 = 1
 20      I2 = MIN (NA, I1+4)
         IF (I1.LE.I2) THEN
            WRITE (HILINE,2020) TSKNAM, (IA(I), I = I1,I2)
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 150
            I1 = I2 + 1
            GO TO 20
            END IF
         HILINE = TSKNAM // '/  IFs replaced'
         CALL HIADD (LUN1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 150
         I1 = 1
 30      I2 = MIN (NB, I1+4)
         IF (I1.LE.I2) THEN
            WRITE (HILINE,2020) TSKNAM, (IB(I), I = I1,I2)
            CALL HIADD (LUN1, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 150
            I1 = I2 + 1
            GO TO 30
            END IF
         END IF
C                                       Close HI file
 150  CALL HICLOS (LUN1, T, BUFF1, IERR)
      GO TO 995
C                                       Error
 990  CALL MSGWRT (8)
C                                       Close down files, etc
 995  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1085 FORMAT ('ERROR ',I4,' COPYING TABLE ',I5)
 1090 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1100 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,'INEXT=''',A2,''' INVERS=',I4,' OUTVERS=', I4)
 2001 FORMAT (A6,'OPTYPE=''',A,''' / what was inverted')
 2020 FORMAT (A6,'FPARM=',5(I3,','))
 2021 FORMAT (A6,'VPARM=',5(I3,','))
      END
      SUBROUTINE CLCOPR (OP, INVER, OUTVER, LUNOLD, LUNNEW, VOL, CNO,
     *   BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a CL table.
C   Inputs:
C      OP       C*4     What to invert 'BOTH', 'PHAS', 'AMPL'
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOL      I       Disk number for both files.
C      CNO      I       Catalog slot number for both files
C   Output:
C      BUFF1    I(512)  Work buffer
C      BUFF2    I(512)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOL, CNO, BUFF1(*),
     *   BUFF2(*), IRET
      CHARACTER OP*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NPOLI,
     *   NUMIF, NTERM, OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OCLRNO, IIF, NPOLO, IREF
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), GMMOD, CR, CI
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION TIME, GEODLY(20)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('CL', VOL, CNO, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open CL file
      CALL CALINI ('READ', BUFF1, VOL, CNO, INVER, CATBLK, LUNOLD,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NPOLI, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      IF (NPOLI.NE.2) THEN
         MSGTXT = 'Input CL table only has one polarization -> R2L'
         CALL MSGWRT (6)
         OP = 'R2L'
         END IF
C                                       # rows in old table
      NCLROW = BUFF1(5)
      NPOLO = 2
C                                       Open up new CL table
      CALL CALINI ('WRIT', BUFF2, VOL, CNO, OUTVER, CATBLK, LUNNEW,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NPOLO, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 800 I = 1,NCLROW
         ICLRNO = I
         CALL TABCAL ('READ', BUFF1, ICLRNO, CLKOLS, CLNUMV, NPOLI,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Switch
         IF (OP.EQ.'SWIT') THEN
            CR = MBDELY(1)
            MBDELY(1) = MBDELY(2)
            MBDELY(2) = CR
            CR = CLOCK(1)
            CLOCK(1) = CLOCK(2)
            CLOCK(2) = CR
            CR = DCLOCK(1)
            DCLOCK(1) = DCLOCK(2)
            DCLOCK(2) = CR
            CR = DISP(1)
            DISP(1) = DISP(2)
            DISP(2) = CR
            CR = DDISP(1)
            DDISP(1) = DDISP(2)
            DDISP(2) = CR
            DO 45 IIF = 1,NUMIF
               CR = CREAL(1,IIF)
               CI = CIMAG(1,IIF)
               CREAL(1,IIF) = CREAL(2,IIF)
               CIMAG(1,IIF) = CIMAG(2,IIF)
               CREAL(2,IIF) = CR
               CIMAG(2,IIF) = CI
               CR = DELAY(1,IIF)
               DELAY(1,IIF) = DELAY(2,IIF)
               DELAY(2,IIF) = CR
               CI = RATE(1,IIF)
               RATE(1,IIF) = RATE(2,IIF)
               RATE(2,IIF) = CI
               IREF = REFA(1,IIF)
               REFA(1,IIF) = REFA(2,IIF)
               REFA(2,IIF) = IREF
               CR = WEIGHT(1,IIF)
               WEIGHT(1,IIF) = WEIGHT(2,IIF)
               WEIGHT(2,IIF) = CR
 45            CONTINUE
C                                       right to left
         ELSE IF (OP.EQ.'R2L') THEN
            MBDELY(2) = MBDELY(1)
            CLOCK(2) = CLOCK(1)
            DCLOCK(2) = DCLOCK(1)
            DISP(2) = DISP(1)
            DDISP(2) = DDISP(1)
            DO 55 IIF = 1,NUMIF
               CREAL(2,IIF) = CREAL(1,IIF)
               CIMAG(2,IIF) = CIMAG(1,IIF)
               DELAY(2,IIF) = DELAY(1,IIF)
               RATE(2,IIF) = RATE(1,IIF)
               REFA(2,IIF) = REFA(1,IIF)
               WEIGHT(2,IIF) = WEIGHT(1,IIF)
 55            CONTINUE
C                                       left to right
         ELSE
            MBDELY(1) = MBDELY(2)
            CLOCK(1) = CLOCK(2)
            DCLOCK(1) = DCLOCK(2)
            DISP(1) = DISP(2)
            DDISP(1) = DDISP(2)
            DO 65 IIF = 1,NUMIF
               CREAL(1,IIF) = CREAL(2,IIF)
               CIMAG(1,IIF) = CIMAG(2,IIF)
               DELAY(1,IIF) = DELAY(2,IIF)
               RATE(1,IIF) = RATE(2,IIF)
               REFA(1,IIF) = REFA(2,IIF)
               WEIGHT(1,IIF) = WEIGHT(2,IIF)
 65            CONTINUE
            END IF
C                                       Write new table.
         CALL TABCAL ('WRIT', BUFF2, OCLRNO, OKOLS, ONUMV, NPOLO,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 800     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFF1, BUFF1, IRET)
      CALL TABIO ('CLOS', 0, OCLRNO, BUFF2, BUFF2, IRET)
      WRITE (MSGTXT,1050) OP, VOL, CNO, INVER, OUTVER
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLCOPR: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CLCOPR: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CLCOPR: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CLCOPR: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('CL copied ',A,' from vol,cno, vers',I3,I5,I4,
     *   ' to vers',I4)
      END
      SUBROUTINE SNCOPR (OP, INVER, OUTVER, LUNOLD, LUNNEW, VOL, CNO,
     *   BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a SN table.
C   Inputs:
C      OP       C*4     What to invert 'BOTH', 'PHAS', 'AMPL'
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOL      I       Disk number for both files.
C      CNO      I       Catalog slot number for both files
C   Output:
C      BUFF1    I(512)  Work buffer
C      BUFF2    I(512)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOL, CNO, BUFF1(*),
     *   BUFF2(*), IRET
      CHARACTER OP*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NPOLI,
     *   NUMIF, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, SOURID, NUMNOD,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OSNRNO, IIF, NODENO, NPOLO,
     *   IREF
      REAL    GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), DISP(2),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), CR, CI, DDISP(2)
      LOGICAL   TABLE, EXIST, FITASC, ISAPPL
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('SN', VOL, CNO, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open SN file
      CALL SNINI ('READ', BUFF1, VOL, CNO, INVER, CATBLK, LUNOLD,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NPOLI, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      IF (NPOLI.NE.2) THEN
         MSGTXT = 'Input SN table only has one polarization -> R2L'
         CALL MSGWRT (6)
         OP = 'R2L'
         END IF
C                                       # rows in old table
      NSNROW = BUFF1(5)
      npolo = 2
C                                       Open up new SN table
      CALL SNINI ('WRIT', BUFF2, VOL, CNO, OUTVER, CATBLK, LUNNEW,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NPOLO, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 800 I = 1,NSNROW
         ISNRNO = I
         CALL TABSN ('READ', BUFF1, ISNRNO, SNKOLS, SNNUMV, NPOLI,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Switch
         IF (OP.EQ.'SWIT') THEN
            CR = MBDELY(1)
            MBDELY(1) = MBDELY(2)
            MBDELY(2) = CR
            CR = DISP(1)
            DISP(1) = DISP(2)
            DISP(2) = CR
            CR = DDISP(1)
            DDISP(1) = DDISP(2)
            DDISP(2) = CR
            DO 45 IIF = 1,NUMIF
               CR = CREAL(1,IIF)
               CI = CIMAG(1,IIF)
               CREAL(1,IIF) = CREAL(2,IIF)
               CIMAG(1,IIF) = CIMAG(2,IIF)
               CREAL(2,IIF) = CR
               CIMAG(2,IIF) = CI
               CR = DELAY(1,IIF)
               DELAY(1,IIF) = DELAY(2,IIF)
               DELAY(2,IIF) = CR
               CI = RATE(1,IIF)
               RATE(1,IIF) = RATE(2,IIF)
               RATE(2,IIF) = CI
               CI = WEIGHT(1,IIF)
               WEIGHT(1,IIF) = WEIGHT(2,IIF)
               WEIGHT(2,IIF) = CI
               IREF = REFA(1,IIF)
               REFA(1,IIF) = REFA(2,IIF)
               REFA(2,IIF) = IREF
 45            CONTINUE
C                                       right to left
         ELSE IF (OP.EQ.'R2L') THEN
            MBDELY(2) = MBDELY(1)
            DISP(2) = DISP(1)
            DDISP(2) = DDISP(1)
            DO 55 IIF = 1,NUMIF
               CREAL(2,IIF) = CREAL(1,IIF)
               CIMAG(2,IIF) = CIMAG(1,IIF)
               DELAY(2,IIF) = DELAY(1,IIF)
               RATE(2,IIF) = RATE(1,IIF)
               REFA(2,IIF) = REFA(1,IIF)
               WEIGHT(2,IIF) = WEIGHT(1,IIF)
 55            CONTINUE
C                                       left to right
         ELSE
            MBDELY(1) = MBDELY(2)
            DISP(1) = DISP(2)
            DDISP(1) = DDISP(2)
            DO 65 IIF = 1,NUMIF
               CREAL(1,IIF) = CREAL(2,IIF)
               CIMAG(1,IIF) = CIMAG(2,IIF)
               DELAY(1,IIF) = DELAY(2,IIF)
               RATE(1,IIF) = RATE(2,IIF)
               REFA(1,IIF) = REFA(2,IIF)
               WEIGHT(1,IIF) = WEIGHT(2,IIF)
 65            CONTINUE
            END IF
C                                       Write new table.
         CALL TABSN ('WRIT', BUFF2, OSNRNO, OKOLS, ONUMV, NPOLO,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 800     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFF1, BUFF1, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, BUFF2, BUFF2, IRET)
      WRITE (MSGTXT,1050) OP, VOL, CNO, INVER, OUTVER
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNINVR: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SNINVR: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SNINVR: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SNINVR: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('SN copied ',A,' from vol, cno, vers',I3,I5,I4,
     *   ' to vers',I4)
      END
      SUBROUTINE CLAVR (NA, IA, NB, IB, INVER, OUTVER, LUNOLD, LUNNEW,
     *   VOL, CNO, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a CL table.
C   Inputs:
C      NA       I       Number in IA
C      IA       I(*)    IFs to average
C      NB       I       Number in IB
C      IB       I(*)    IFs to replace
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOL      I       Disk number for both files.
C      CNO      I       Catalog slot number for both files
C   Output:
C      BUFF1    I(512)  Work buffer
C      BUFF2    I(512)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C-----------------------------------------------------------------------
      INTEGER   NA, IA(*), NB, IB(*), INVER, OUTVER, LUNOLD, LUNNEW,
     *   VOL, CNO, BUFF1(*), BUFF2(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NPOLI,
     *   NUMIF, NTERM, OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OCLRNO, NPOLO, IREF,
     *   NS(5), J, K, IPOL
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), GMMOD, SUMS(5)
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION TIME, GEODLY(20)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('CL', VOL, CNO, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open CL file
      CALL CALINI ('READ', BUFF1, VOL, CNO, INVER, CATBLK, LUNOLD,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NPOLI, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NCLROW = BUFF1(5)
      NPOLO = NPOLI
C                                       Open up new CL table
      CALL CALINI ('WRIT', BUFF2, VOL, CNO, OUTVER, CATBLK, LUNNEW,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NPOLO, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 K = 1,NCLROW
         ICLRNO = K
         CALL TABCAL ('READ', BUFF1, ICLRNO, CLKOLS, CLNUMV, NPOLI,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 100
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       sum
         DO 30 IPOL = 1,NPOLI
            CALL FILL (5, 0, NS)
            CALL RFILL (5, 0.0, SUMS)
            DO 10 I = 1,NA
               J = IA(I)
               IF ((WEIGHT(IPOL,J).GT.0.0) .AND.
     *            (WEIGHT(IPOL,J).NE.FBLANK)) THEN
                  IF (CREAL(IPOL,J).NE.FBLANK) THEN
                     SUMS(1) = SUMS(1) + CREAL(IPOL,J)
                     NS(1) = NS(1) + 1
                     END IF
                  IF (CIMAG(IPOL,J).NE.FBLANK) THEN
                     SUMS(2) = SUMS(2) + CIMAG(IPOL,J)
                     NS(2) = NS(2) + 1
                     END IF
                  IF (DELAY(IPOL,J).NE.FBLANK) THEN
                     SUMS(3) = SUMS(3) + DELAY(IPOL,J)
                     NS(3) = NS(3) + 1
                     END IF
                  IF (RATE(IPOL,J).NE.FBLANK) THEN
                     SUMS(4) = SUMS(4) + RATE(IPOL,J)
                     NS(4) = NS(4) + 1
                     END IF
                  SUMS(5) = SUMS(5) + WEIGHT(IPOL,J)
                  NS(5) = NS(5) + 1
                  END IF
 10            CONTINUE
C                                       average
            IF (NS(1).GT.0) THEN
               SUMS(1) = SUMS(1) / NS(1)
            ELSE
               SUMS(1) = FBLANK
               END IF
            IF (NS(2).GT.0) THEN
               SUMS(2) = SUMS(2) / NS(2)
            ELSE
               SUMS(2) = FBLANK
               END IF
            IF (NS(3).GT.0) THEN
               SUMS(3) = SUMS(3) / NS(3)
            ELSE
               SUMS(3) = FBLANK
               END IF
            IF (NS(4).GT.0) THEN
               SUMS(4) = SUMS(4) / NS(4)
            ELSE
               SUMS(4) = FBLANK
               END IF
            IF (NS(5).GT.0) THEN
               SUMS(5) = SUMS(5) / NS(5)
            ELSE
               SUMS(5) = FBLANK
               END IF
            IREF = REFA(IPOL,IA(1))
C                                       replace
            DO 20 I = 1,NB
               J = IB(I)
               REFA(IPOL,J) = IREF
               CREAL(IPOL,J) = SUMS(1)
               CIMAG(IPOL,J) = SUMS(2)
               DELAY(IPOL,J) = SUMS(3)
               RATE(IPOL,J)  = SUMS(4)
               WEIGHT(IPOL,J) = SUMS(5)
 20            CONTINUE
 30         CONTINUE
C                                       Write new table.
         CALL TABCAL ('WRIT', BUFF2, OCLRNO, OKOLS, ONUMV, NPOLO,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFF1, BUFF1, IRET)
      CALL TABIO ('CLOS', 0, OCLRNO, BUFF2, BUFF2, IRET)
      WRITE (MSGTXT,1050) 'AVER', VOL, CNO, INVER, OUTVER
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLAVR: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CLAVR: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CLAVR: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CLAVR: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('CL copied ',A,' from vol,cno, vers',I3,I5,I4,
     *   ' to vers',I4)
      END
      SUBROUTINE SNAVR (NA, IA, NB, IB, INVER, OUTVER, LUNOLD, LUNNEW,
     *   VOL, CNO, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a SN table.
C   Inputs:
C      NA       I       Number in IA
C      IA       I(*)    IFs to average
C      NB       I       Number in IB
C      IB       I(*)    IFs to replace
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOL      I       Disk number for both files.
C      CNO      I       Catalog slot number for both files
C   Output:
C      BUFF1    I(512)  Work buffer
C      BUFF2    I(512)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C-----------------------------------------------------------------------
      INTEGER   NA, IA(*), NB, IB(*), INVER, OUTVER, LUNOLD, LUNNEW,
     *   VOL, CNO, BUFF1(*), BUFF2(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NPOLI,
     *   NUMIF, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, SOURID, NUMNOD,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OSNRNO, NODENO, NPOLO,
     *   IREF, NS(5), J, K, IPOL
      REAL    GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), DISP(2),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), SUMS(5), DDISP(2)
      LOGICAL   TABLE, EXIST, FITASC, ISAPPL
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('SN', VOL, CNO, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open SN file
      CALL SNINI ('READ', BUFF1, VOL, CNO, INVER, CATBLK, LUNOLD,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NPOLI, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSNROW = BUFF1(5)
      NPOLO = NPOLI
C                                       Open up new SN table
      CALL SNINI ('WRIT', BUFF2, VOL, CNO, OUTVER, CATBLK, LUNNEW,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NPOLO, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 K = 1,NSNROW
         ISNRNO = K
         CALL TABSN ('READ', BUFF1, ISNRNO, SNKOLS, SNNUMV, NPOLI,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 100
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       sum
         DO 30 IPOL = 1,NPOLI
            CALL FILL (5, 0, NS)
            CALL RFILL (5, 0.0, SUMS)
            DO 10 I = 1,NA
               J = IA(I)
               IF ((WEIGHT(IPOL,J).GT.0.0) .AND.
     *            (WEIGHT(IPOL,J).NE.FBLANK)) THEN
                  IF (CREAL(IPOL,J).NE.FBLANK) THEN
                     SUMS(1) = SUMS(1) + CREAL(IPOL,J)
                     NS(1) = NS(1) + 1
                     END IF
                  IF (CIMAG(IPOL,J).NE.FBLANK) THEN
                     SUMS(2) = SUMS(2) + CIMAG(IPOL,J)
                     NS(2) = NS(2) + 1
                     END IF
                  IF (DELAY(IPOL,J).NE.FBLANK) THEN
                     SUMS(3) = SUMS(3) + DELAY(IPOL,J)
                     NS(3) = NS(3) + 1
                     END IF
                  IF (RATE(IPOL,J).NE.FBLANK) THEN
                     SUMS(4) = SUMS(4) + RATE(IPOL,J)
                     NS(4) = NS(4) + 1
                     END IF
                  SUMS(5) = SUMS(5) + WEIGHT(IPOL,J)
                  NS(5) = NS(5) + 1
                  END IF
 10            CONTINUE
C                                       average
            IF (NS(1).GT.0) THEN
               SUMS(1) = SUMS(1) / NS(1)
            ELSE
               SUMS(1) = FBLANK
               END IF
            IF (NS(2).GT.0) THEN
               SUMS(2) = SUMS(2) / NS(2)
            ELSE
               SUMS(2) = FBLANK
               END IF
            IF (NS(3).GT.0) THEN
               SUMS(3) = SUMS(3) / NS(3)
            ELSE
               SUMS(3) = FBLANK
               END IF
            IF (NS(4).GT.0) THEN
               SUMS(4) = SUMS(4) / NS(4)
            ELSE
               SUMS(4) = FBLANK
               END IF
            IF (NS(5).GT.0) THEN
               SUMS(5) = SUMS(5) / NS(5)
            ELSE
               SUMS(5) = FBLANK
               END IF
            IREF = REFA(IPOL,IA(1))
C                                       replace
            DO 20 I = 1,NB
               J = IB(I)
               REFA(IPOL,J) = IREF
               CREAL(IPOL,J) = SUMS(1)
               CIMAG(IPOL,J) = SUMS(2)
               DELAY(IPOL,J) = SUMS(3)
               RATE(IPOL,J)  = SUMS(4)
               WEIGHT(IPOL,J) = SUMS(5)
 20            CONTINUE
 30         CONTINUE
C                                       Write new table.
         CALL TABSN ('WRIT', BUFF2, OSNRNO, OKOLS, ONUMV, NPOLO,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFF1, BUFF1, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, BUFF2, BUFF2, IRET)
      WRITE (MSGTXT,1050) 'AVER', VOL, CNO, INVER, OUTVER
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNAVR: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SNAVR: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SNAVR: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SNAVR: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('SN copied ',A,' from vol, cno, vers',I3,I5,I4,
     *   ' to vers',I4)
      END
