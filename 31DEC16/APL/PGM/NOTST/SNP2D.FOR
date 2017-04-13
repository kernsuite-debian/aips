LOCAL INCLUDE 'SNP2D.INC'
      HOLLERITH XNAMIN(3), XCLAIN(2), XNMOUT(3), XCLOUT(2)
      REAL      XSEQIN, XDISKI, XIVER, XSEQO, XDISKO, XBIF, XBCHAN,
     *   XDOAPP
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XIVER, XNMOUT,
     *   XCLOUT, XSEQO, XDISKO, XBIF, XBCHAN, XDOAPP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      INTEGER   CATOLD(256), SEQIN, DISKIN, SEQOUT, DISKOU, OLDCNO,
     *   NEWCNO, BCHAN, BIF, IVER, OVER
      REAL      OLDR(256)
      COMMON /OTHERP/ CATOLD, SEQIN, DISKIN, SEQOUT, DISKOU, OLDCNO,
     *   NEWCNO, BCHAN, BIF, IVER, OVER
      COMMON /OTHERC/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      EQUIVALENCE (CATOLD, OLDR)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM SNP2D
C-----------------------------------------------------------------------
C! Task to copy SN table converting phase to delay
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2015
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
C  SNP2D copies AIPS an SN extension tables file averaging IFs.
C  Adverbs:
C    INNAME      Input image name (name)
C    INCLASS     Input image name (class)
C    INSEQ       Input image name (seq. #)
C    INDISK      Input image disk unit #
C    INVERS      Input table file version no.
C    OUTNAME     Output image name (name)
C    OUTCLASS    Output image name (class)
C    OUTSEQ      Output image name (seq. #)
C    OUTDISK     Output image disk unit #.
C    BIF         IF of cal channel
C    BCHAN       channel of cal in BIF
C    DOAPPLY     > 0 =< cary amplitude, else make ampl 1.0
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNP2D.INC'
      CHARACTER FTYPE*2, STAT*4, XEXT*2, PRGM*6, TABTYP*2, ATIME*8,
     *   ADATE*12, HILINE*72
      INTEGER  IRET, BUFF1(512), BUFF2(512), IERR, LUN1, LUN2, NPARM,
     *   IROUND,  NTAB, NOLDT, TIME(3), DATE(3), NIF, NIFO
      LOGICAL   EXIST, TABLE, FITASC
      LOGICAL   T, SAME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'SNP2D '/
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
      XEXT = 'SN'
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKOU = IROUND (XDISKO)
      IVER   = IROUND (XIVER)
      OVER = 0
      TABTYP = XEXT
C                                       Find input
      OLDCNO = 1
      FTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, FTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Default output = input.
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') CLAOUT = CLAIN
      IF (SEQOUT.LE.0) SEQOUT = SEQIN
      IF (DISKOU.EQ.0) DISKOU = DISKIN
C                                       Find output
      NEWCNO = 1
      FTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKOU, NEWCNO, NAMOUT, CLAOUT, SEQOUT,
     *    FTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMOUT, CLAOUT, SEQOUT, DISKOU,
     *      NLUSER
         GO TO 990
         END IF
      SAME = (DISKIN.EQ.DISKOU) .AND. (OLDCNO.EQ.NEWCNO)
      STAT = 'READ'
      IF (SAME) STAT = 'REST'
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, STAT, BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CALL COPY (256, CATBLK, CATOLD)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UVPGET FAILS ON INPUT UV DATA SET'
         GO TO 990
         END IF
      IF (JLOCIF.GE.0) THEN
         NIF = CATBLK (KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
      BIF = XBIF + 0.1
      BCHAN = XBCHAN + 0.1
      BIF = MAX (1, MIN (NIF, BIF))
      BCHAN = MAX (1, MIN (CATBLK(KINAX+JLOCF), BCHAN))
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKOU, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 1
C                                       Check number of tables
      CALL FNDEXT (TABTYP, CATOLD, NOLDT)
C                                       use highest version when IVER = 0
      IF (IVER.LE.0) IVER = NOLDT
      XIVER = IVER
      NTAB = 1
C                                       Is it a table
      CALL ISTAB (TABTYP, DISKIN, OLDCNO, IVER, LUN1, BUFF1, TABLE,
     *   EXIST, FITASC, IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      IF (.NOT.EXIST) THEN
         IRET = 5
         WRITE (MSGTXT,1031) TABTYP, IVER
         GO TO 990
         END IF
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UVPGET FAILS ON OUTPUT UV DATA SET'
         GO TO 990
         END IF
      IF (JLOCIF.GE.0) THEN
         NIFO = CATBLK (KINAX+JLOCIF)
      ELSE
         NIFO = 1
         END IF
C                                       Copy table
      CALL SNP2DY (LUN1, LUN2, NIFO, BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKOU, NEWCNO, BUFF1, IERR)
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
      WRITE (HILINE,2020) TSKNAM, NIFO
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
      IF (XDOAPP.GT.0.0) THEN
         HILINE = TSKNAM // '/ amplitudes preserved'
      ELSE
         HILINE = TSKNAM // '/ amplitudes set to 1.0'
         END IF
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
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
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1030 FORMAT ('ISTAB ERROR ',I3,' CHECKING FILE')
 1031 FORMAT ('Extension file ',A2,' version',I5,' missing')
 1086 FORMAT ('TBLCOP ERROR ',I4,' COPYING EXT FILE ',A2,I5)
 1090 FORMAT ('ERROR',I3,' OPENING HISTORY FILE')
 1100 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,'INEXT=''',A2,''' INVERS=',I4,' OUTVERS=', I4)
 2020 FORMAT (A6,'NIFOUT =',I3,' / Number output IFs')
      END
      SUBROUTINE SNP2DY (LUNOLD, LUNNEW, NIFO, BUFOLD, BUFNEW, IRET)
C-----------------------------------------------------------------------
C   SNP2DY copies an SN table averaging specified IFs.
C   Inputs:
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      NIFO     I       Number of IFs out
C   Output:
C      BUFOLD   I(512)  Work buffer
C      BUFNEW   I(512)  Work buffer
C      IRET     I       Return error code  0 => ok
C                          1 => files the same, no copy.
C                          2 => no input files exist
C                          3 => failed
C                          4 => no output files created.
C                          5 => failed to update CATNEW
C                          6 => output file exists
C   Inputs from MSG common:
C      MSGSUP   I       If 31990 < MSGSUP < 32000, the file copied
C                       message is suppressed.
C-----------------------------------------------------------------------
      INTEGER   LUNOLD, LUNNEW, BUFOLD(512), BUFNEW(512), NIFO, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNP2D.INC'
      INTEGER   I, NREC, SNKOLS(MAXSNC), SNNUMV(MAXSNC), SNKOLO(MAXSNC),
     *   SNNUMO(MAXSNC), ISNRNO, OSNRNO, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), REF, IREC,
     *   LSTSOU, GUSE, J, LUN, LUNTMP, VER, NIF
      LOGICAL   ISAPPL, CLSORT
      DOUBLE PRECISION TIME, THEFRQ, FRQOFF(MAXIF), FREQ0
      REAL      TIMEI, IFR, MBDELY(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), GMMOD, UT,
     *   RANOD(25), DECNOD(25), APARM(10), PHASE, AMP, WT, DDELAY,
     *   DISP(2), DDISP(2)
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DCHND.INC'
      DATA APARM /10 * 0.0/
C-----------------------------------------------------------------------
C                                       get output FREQS
      CALL FNDEXT ('FQ', CATBLK, VER)
      LUN = LUNTMP (1)
      FQCHND = 1
      CALL CHNDAT ('READ', BUFNEW, DISKOU, NEWCNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FQCHND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING OUTPUT FREQUENCY TABLE'
         GO TO 990
         END IF
      FREQ0 = CATD(KDCRV+JLOCF)
C                                       highest CL/FO
      GUSE = 0
C                                       Open input
      CALL SNINI ('READ', BUFOLD, DISKIN, OLDCNO, IVER, CATOLD, LUNOLD,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT SN TABLE'
         GO TO 990
         END IF
C                                       create output
      OVER = 0
      CALL SNINI ('WRIT', BUFNEW, DISKOU, NEWCNO, OVER, CATBLK,
     *   LUNNEW, OSNRNO, SNKOLO, SNNUMO, NUMANT, NUMPOL, NIFO, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT SN TABLE'
         GO TO 990
         END IF
      NREC = BUFOLD(5)
      LSTSOU = -1
C                                       read/write loop
      DO 50 IREC = 1,NREC
         ISNRNO = IREC
         CALL TABSN ('READ', BUFOLD, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) THEN
            IF (IRET.LT.0) GO TO 50
            WRITE (MSGTXT,1000) IRET, 'READING INPUT SN TABLE'
            GO TO 990
            END IF
C                                       get current freq info
         IF (LSTSOU.NE.SOURID) THEN
            I = MSGKIL
            MSGKIL = 32005
            CALL GETFRQ (DISKIN, OLDCNO, APARM, LSTSOU, SOURID, FREQID,
     *         CATOLD, IRET)
            MSGKIL = I
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'GETTING BASIC FREQ INFO'
               GO TO 990
               END IF
            END IF
         LSTSOU= SOURID
C                                       Obtain any other frq. offsets
C                                       hiding in the CL table
         UT = TIME - (IATUT/86400.0D0)
         I = MSGKIL
         MSGKIL = 32005
         CALL FRQUPD (DISKIN, OLDCNO, GUSE, ANTNO, UT, LSTSOU, SUBA,
     *      FREQID, CATOLD, CLSORT, FRQOFF, IRET)
         CLSORT = .TRUE.
         MSGKIL = I
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING DOPPLER FREQ OFFSET'
            GO TO 990
            END IF
         THEFRQ = REFFRQ(BIF) + FRQOFF(BIF) + (BCHAN-OLDR(KRCRP+JLOCF))
     *      * CFINC(BIF)
C                                       get values
         DO 40 I = 1,2
            REF = REFA(I,BIF)
            IF ((CREAL(I,BIF).EQ.FBLANK) .OR. (CIMAG(I,BIF).EQ.FBLANK))
     *         THEN
               DDELAY = FBLANK
               AMP   = FBLANK
               WT = 0.0
            ELSE
               WT = WEIGHT(I,BIF)
               PHASE = ATAN2 (CIMAG(I,BIF), CREAL(I,BIF))
               DDELAY = PHASE / TWOPI / THEFRQ
               IF (XDOAPP.GT.0.0) THEN
                  AMP = SQRT (CREAL(I,BIF)*CREAL(I,BIF) +
     *               CIMAG(I,BIF)*CIMAG(I,BIF))
               ELSE
                  AMP = 1.0
                  END IF
               END IF
            PHASE = 0.0
            DO 30 J = 1,NIFO
               IF (AMP.NE.FBLANK) THEN
                  PHASE = TWOPI * (FOFF(J) + FREQ0) * DDELAY
                  CREAL(I,J) = AMP * COS (PHASE)
                  CIMAG(I,J) = AMP * SIN (PHASE)
               ELSE
                  CREAL(I,J) = AMP
                  CIMAG(I,J) = 0.0
                  END IF
               DELAY(I,J) = DDELAY
               RATE(I,J) = 0.0
               WEIGHT(I,J) = WT
               REFA(I,J) = REF
 30            CONTINUE
 40         CONTINUE
         OSNRNO = IREC
         CALL TABSN ('WRIT', BUFNEW, OSNRNO, SNKOLO, SNNUMO, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT SN TABLE'
            GO TO 990
            END IF
 50      CONTINUE
      CALL TABSN ('CLOS', BUFOLD, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *   TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING INPUT SN TABLE'
         CALL MSGWRT (8)
         END IF
      CALL TABSN ('CLOS', BUFNEW, OSNRNO, SNKOLO, SNNUMO, NUMPOL,
     *   TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING OUTPUT SN TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNP2DY ERROR',I4,' DOING ',A)
      END
