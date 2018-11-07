      PROGRAM CLINV
C-----------------------------------------------------------------------
C! Task to invert calibration in SN/CL tables
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2008-2009, 2015
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
C  CLINV copies AIPS SN/CL extension files, inverting some or all of the
C  calibration.  The specified output table should not exist before the
C  execution of CLINV.
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
C    OPTYPE      'PHAS', 'AMPL', else both
C-----------------------------------------------------------------------
      HOLLERITH XNAMIN(3), XCLAIN(2), XNMOUT(3), XCLOUT(2), XNEXT,
     *   CATHLD(256), XOPTYP
      CHARACTER  NAMEIN*12, CLAIN*6, FTYPE*2, STAT*4, NAMOUT*12,
     *   CLAOUT*6, OPTYPE*4, PRGM*6, TABTYP*2, ATIME*8, ADATE*12
      INTEGER  IRET, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), BUFF1(512), BUFF2(512), IERR, IVER, OVER, LUN1,
     *   LUN2, NPARM, IROUND, NOLDT, TIME(3), DATE(3)
      REAL      XSEQIN, XDISKI, XIVER, XSEQO, XDISKO, XOVER
      LOGICAL   T, SAME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XNEXT, XIVER,
     *   XNMOUT, XCLOUT, XSEQO, XDISKO, XOVER, XOPTYP
      EQUIVALENCE (CATOLD, CATHLD)
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'CLINV '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 18
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
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLAOUT)
      CALL H2CHR (2, 1, XNEXT, TABTYP)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF ((OPTYPE.NE.'PHAS') .AND. (OPTYPE.NE.'AMPL')) OPTYPE = 'BOTH'
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO  = IROUND (XDISKO)
      IVER   = IROUND (XIVER)
      OVER   = IROUND (XOVER)
C                                       Default output = input.
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') CLAOUT = CLAIN
      IF (TABTYP.NE.'CL') TABTYP = 'SN'
C                                       Find input
 30   OLDCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Find output
      NEWCNO = 1
      FTYPE = ' '
      CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT, FTYPE,
     *    NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *      NLUSER
         GO TO 990
         END IF
      SAME = (DISKIN.EQ.DISKO) .AND. (OLDCNO.EQ.NEWCNO)
      STAT = 'READ'
      IF (SAME) STAT = 'REST'
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, STAT, BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 1
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATOLD, NOLDT)
C                                       use highest version if IVER = 0
      IF (IVER.LE.0) IVER = NOLDT
C                                       Copy table
      IF (TABTYP.EQ.'CL') THEN
         CALL CLINVR (OPTYPE, IVER, OVER, LUN1, LUN2, DISKIN, DISKO,
     *      OLDCNO, NEWCNO, CATOLD, CATBLK, BUFF1, BUFF2, IRET)
      ELSE
         CALL SNINVR (OPTYPE, IVER, OVER, LUN1, LUN2, DISKIN, DISKO,
     *      OLDCNO, NEWCNO, CATOLD, CATBLK, BUFF1, BUFF2, IRET)
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
      CALL HIOPEN (LUN1, DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         GO TO 150
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (MSGTXT,1100) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 150
C                                       Type and version
      WRITE (MSGTXT,2000) TSKNAM, TABTYP, IVER, OVER
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
C                                       OPTYPE
      WRITE (MSGTXT,2001) TSKNAM, OPTYPE
      CALL HIADD (LUN1, MSGTXT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
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
      END
      SUBROUTINE CLINVR (OP, INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATOLD, CATNEW, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a CL table.
C   Inputs:
C      OP       C*4     What to invert 'BOTH', 'PHAS', 'AMPL'
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOLOLD   I       Disk number for old file.
C      VOLNEW   I       Disk number for new file.
C      CNOOLD   I       Catalog slot number for old file
C      CNONEW   I       Catalog slot number for new file
C      CATOLD   I       Catalog header for old file
C   In/out:
C      CATNEW   I(256)  Catalog header for new file.
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
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, BUFF1(256), BUFF2(256), CATOLD(256), CATNEW(256), IRET
      CHARACTER OP*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NUMPOL,
     *   NUMIF, NTERM, OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OCLRNO, IIF, IIP, NNP
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), GMMOD, CR, CI, CA
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION TIME, GEODLY(20)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('CL', VOLNEW, CNONEW, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open CL file
      CALL CALINI ('READ', BUFF1, VOLOLD, CNOOLD, INVER, CATOLD, LUNOLD,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NCLROW = BUFF1(5)
      NNP = MIN (2, NUMPOL)
C                                       Open up new CL table
      CALL CALINI ('WRIT', BUFF2, VOLNEW, CNONEW, OUTVER, CATNEW,
     *   LUNNEW, OCLRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NTERM,
     *   GMMOD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 800 I = 1,NCLROW
         ICLRNO = I
         CALL TABCAL ('READ', BUFF1, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Phase only
         IF (OP.EQ.'PHAS') THEN
            DO 45 IIF = 1,NUMIF
               DO 40 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CA = SQRT (CA)
                        CREAL(IIP,IIF) = CR / CA
                        CIMAG(IIP,IIF) = -CI / CA
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        CIMAG(IIP,IIF) = FBLANK
                        END IF
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK)
     *               DELAY(IIP,IIF) = -DELAY(IIP,IIF)
                  IF (RATE(IIP,IIF).NE.FBLANK)
     *               RATE(IIP,IIF) = -RATE(IIP,IIF)
 40               CONTINUE
 45            CONTINUE
C                                       Amplitude only
         ELSE IF (OP.EQ.'AMPL') THEN
            DO 55 IIF = 1,NUMIF
               DO 50 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CREAL(IIP,IIF) = 1.0 / SQRT (CA)
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        END IF
                     CIMAG(IIP,IIF) = 0.0
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK) DELAY(IIP,IIF) = 0.0
                  IF (RATE(IIP,IIF).NE.FBLANK) RATE(IIP,IIF) = 0.0
 50               CONTINUE
 55            CONTINUE
C                                       Both
         ELSE
            DO 65 IIF = 1,NUMIF
               DO 60 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CREAL(IIP,IIF) = CR / CA
                        CIMAG(IIP,IIF) = -CI / CA
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        CIMAG(IIP,IIF) = FBLANK
                        END IF
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK)
     *               DELAY(IIP,IIF) = -DELAY(IIP,IIF)
                  IF (RATE(IIP,IIF).NE.FBLANK)
     *               RATE(IIP,IIF) = -RATE(IIP,IIF)
 60               CONTINUE
 65            CONTINUE
            END IF
C                                       Write new table.
         CALL TABCAL ('WRIT', BUFF2, OCLRNO, OKOLS, ONUMV, NUMPOL,
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
      WRITE (MSGTXT,1050) OP, VOLOLD, CNOOLD, INVER, VOLNEW, CNONEW,
     *   OUTVER
      CALL MSGWRT (3)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLINVR: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CLINVR: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CLINVR: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CLINVR: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('Inverted CL ',A,' from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
      END
      SUBROUTINE SNINVR (OP, INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATOLD, CATNEW, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   Routine to invert the calibration of a SN table.
C   Inputs:
C      OP       C*4     What to invert 'BOTH', 'PHAS', 'AMPL'
C      INVER    I       Version number to copy.
C      OUTVER   I       Version number on output file
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOLOLD   I       Disk number for old file.
C      VOLNEW   I       Disk number for new file.
C      CNOOLD   I       Catalog slot number for old file
C      CNONEW   I       Catalog slot number for new file
C      CATOLD   I       Catalog header for old file
C   In/out:
C      CATNEW   I(256)  Catalog header for new file.
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
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, BUFF1(256), BUFF2(256), CATOLD(256), CATNEW(256), IRET
      CHARACTER OP*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, SOURID, NUMNOD,
     *   ANTNO, SUBA, FREQID, REFA(2,MAXIF), OSNRNO, IIF, IIP, NODENO,
     *   NNP
      REAL    GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), DISP(2),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), CR, CI, CA, DDISP(2)
      LOGICAL   TABLE, EXIST, FITASC, ISAPPL
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      IF (OUTVER.GT.0) THEN
         CALL ISTAB ('SN', VOLNEW, CNONEW, OUTVER, LUNNEW, BUFF2, TABLE,
     *      EXIST, FITASC, IRET)
         IF (EXIST) THEN
            IRET = 6
            GO TO 999
            END IF
         END IF
C                                       Open SN file
      CALL SNINI ('READ', BUFF1, VOLOLD, CNOOLD, INVER, CATOLD, LUNOLD,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSNROW = BUFF1(5)
      NNP = MIN (2, NUMPOL)
C                                       Open up new SN table
      CALL SNINI ('WRIT', BUFF2, VOLNEW, CNONEW, OUTVER, CATNEW,
     *   LUNNEW, OSNRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 800 I = 1,NSNROW
         ISNRNO = I
         CALL TABSN ('READ', BUFF1, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Phase only
         IF (OP.EQ.'PHAS') THEN
            DO 45 IIF = 1,NUMIF
               DO 40 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CA = SQRT (CA)
                        CREAL(IIP,IIF) = CR / CA
                        CIMAG(IIP,IIF) = -CI / CA
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        CIMAG(IIP,IIF) = FBLANK
                        END IF
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK)
     *               DELAY(IIP,IIF) = -DELAY(IIP,IIF)
                  IF (RATE(IIP,IIF).NE.FBLANK)
     *               RATE(IIP,IIF) = -RATE(IIP,IIF)
 40               CONTINUE
 45            CONTINUE
C                                       Amplitude only
         ELSE IF (OP.EQ.'AMPL') THEN
            DO 55 IIF = 1,NUMIF
               DO 50 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CREAL(IIP,IIF) = 1.0 / SQRT (CA)
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        END IF
                     CIMAG(IIP,IIF) = 0.0
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK) DELAY(IIP,IIF) = 0.0
                  IF (RATE(IIP,IIF).NE.FBLANK) RATE(IIP,IIF) = 0.0
 50               CONTINUE
 55            CONTINUE
C                                       Both
         ELSE
            DO 65 IIF = 1,NUMIF
               DO 60 IIP = 1,NNP
                  IF ((CREAL(IIP,IIF).NE.FBLANK) .AND.
     *               (CIMAG(IIP,IIF).NE.FBLANK)) THEN
                     CR = CREAL(IIP,IIF)
                     CI = CIMAG(IIP,IIF)
                     CA = CR*CR+CI*CI
                     IF (CA.GT.0.0) THEN
                        CREAL(IIP,IIF) = CR / CA
                        CIMAG(IIP,IIF) = -CI / CA
                     ELSE
                        CREAL(IIP,IIF) = FBLANK
                        CIMAG(IIP,IIF) = FBLANK
                        END IF
                     END IF
                  IF (DELAY(IIP,IIF).NE.FBLANK)
     *               DELAY(IIP,IIF) = -DELAY(IIP,IIF)
                  IF (RATE(IIP,IIF).NE.FBLANK)
     *               RATE(IIP,IIF) = -RATE(IIP,IIF)
 60               CONTINUE
 65            CONTINUE
            END IF
C                                       Write new table.
         CALL TABSN ('WRIT', BUFF2, OSNRNO, OKOLS, ONUMV, NUMPOL,
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
      WRITE (MSGTXT,1050) OP, VOLOLD, CNOOLD, INVER, VOLNEW, CNONEW,
     *   OUTVER
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
 1050 FORMAT ('Inverted SN ',A,' from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
      END
