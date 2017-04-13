      PROGRAM SNCOP
C-----------------------------------------------------------------------
C! Task to copy SN table averaging input IFs
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2012, 2015
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
C  SNCOP copies AIPS an SN extension tables file averaging IFs.
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
C    DPARM       List of IFs to average
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NAMEIN*12, CLAIN*6, FTYPE*2, STAT*4, NAMOUT*12,
     *   CLAOUT*6, XEXT*2, PRGM*6, TABTYP*2, ATIME*8, ADATE*12,
     *   HILINE*72
      HOLLERITH XNAMIN(3), XCLAIN(2), XNMOUT(3), XCLOUT(2)
      INTEGER  IRET, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, I,
     *   CATOLD(256), BUFF1(512), BUFF2(512), IERR, IVER, OVER, LUN1,
     *   LUN2, NPARM, IROUND,  NTAB, NOLDT, TIME(3), DATE(3), NIF, J,
     *   IF2AVG(MAXIF), I1, I2, NBOX
      LOGICAL   EXIST, TABLE, FITASC
      REAL      XSEQIN, XDISKI, XIVER, XSEQO, XDISKO, XBOX(2,20)
      LOGICAL   T, SAME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XIVER, XNMOUT,
     *   XCLOUT, XSEQO, XDISKO, XBOX
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA PRGM /'SNCOP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 55
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
      DISKO  = IROUND (XDISKO)
      IVER   = IROUND (XIVER)
      OVER = 0
      TABTYP = XEXT
C                                       Prevent write to disk # 0 !
      IF (DISKO.EQ.0) DISKO = DISKIN
C                                       Default output = input.
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      IF (CLAOUT.EQ.' ') CLAOUT = CLAIN
      IF (SEQOUT.LE.0) SEQOUT = SEQIN
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
C                                       Find output
      NEWCNO = 1
      FTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT, FTYPE,
     *    NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *      NLUSER
         GO TO 990
         END IF
      SAME = (DISKIN.EQ.DISKO) .AND. (OLDCNO.EQ.NEWCNO)
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
      IF (.NOT.SAME) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
C                                       Read new CATBLK and mark 'WRIT'
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
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
C                                       which IFs to average
      NBOX = 0
      CALL FILL (MAXIF, 0, IF2AVG)
      DO 50 J = 1,20
         I1 = IROUND (XBOX(1,J))
         I2 = IROUND (XBOX(2,J))
         IF ((I1.GT.0) .AND. (I2.GE.I1)) THEN
            DO 40 I = I1,I2
               IF2AVG(I) = 1
 40            CONTINUE
            NBOX = NBOX + 1
         ELSE
            GO TO 60
            END IF
 50      CONTINUE
 60   IF (NBOX.EQ.0) THEN
         NBOX = 1
         XBOX(1,1) = 1.0
         XBOX(2,1) = NIF
         CALL FILL (NIF, 1, IF2AVG)
         END IF
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UVPGET FAILS ON OUTPUT UV DATA SET'
         GO TO 990
         END IF
      IF (JLOCIF.GE.0) THEN
         NIF = CATBLK (KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
C                                       Copy table
      CALL SNCOPY (IVER, LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO,
     *   CATOLD, CATBLK, IF2AVG, NIF, OVER, BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 995
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
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
      DO 70 J = 1,NBOX
         I1 = IROUND (XBOX(1,J))
         I2 = IROUND (XBOX(2,J))
         WRITE (HILINE,2010) TSKNAM, J, I1, I2
         CALL HIADD (LUN1, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 150
         MSGTXT = HILINE(7:)
         CALL MSGWRT (3)
 70      CONTINUE
      WRITE (HILINE,2020) TSKNAM, NIF
      CALL HIADD (LUN1, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 150
      MSGTXT = HILINE(7:)
      CALL MSGWRT (3)
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
 2010 FORMAT (A6,'IF2AVG(',I2,') =',I3,' ,',I3,' / IFs averaged')
 2020 FORMAT (A6,'NIFOUT =',I3,' / Number output IFs')
      END
      SUBROUTINE SNCOPY (INVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, CATOLD, CATNEW, IF2AVG, NIFO, OUTVER, BUFOLD, BUFNEW,
     *   IRET)
C-----------------------------------------------------------------------
C   SNCOPY copies an SN table averaging specified IFs.
C   Inputs:
C      INVER    I       Version number to copy, 0 => copy all.
C      LUNOLD   I       LUN for old file
C      LUNNEW   I       LUN for new file
C      VOLOLD   I       Disk number for old file.
C      VOLNEW   I       Disk number for new file.
C      CNOOLD   I       Catalog slot number for old file
C      CNONEW   I       Catalog slot number for new file
C      IF2FLG   I(*)    > 0 => include this IF in average
C      NIFO     I       Number of IFs out
C   In/out:
C      CATNEW   I(256)  Catalog header for new file.
C   Output:
C      OUTVER   I       Version number on output file, if more than one
C                       copied (INVER=0) this will be the number of the
C                       first file.  If OUTVER = 0, it will be taken as
C                       1 higher than the previous highest version.
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
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, BUFOLD(512), BUFNEW(512), CATOLD(256), CATNEW(256),
     *   NIFO, IF2AVG(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, NREC, SNKOLS(MAXSNC), SNNUMV(MAXSNC), SNKOLO(MAXSNC),
     *   SNNUMO(MAXSNC), ISNRNO, OSNRNO, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), NAVG1,
     *   NAVG2, REF1, REF2, IREC
      LOGICAL   ISAPPL
      DOUBLE PRECISION TIME
      REAL      TIMEI, IFR, MBDELY(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), GMMOD,
     *   RANOD(25), DECNOD(25), DAVG(2,5), DISP(2), DDISP(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       OPEN INPUT
      CALL SNINI ('READ', BUFOLD, VOLOLD, CNOOLD, INVER, CATOLD, LUNOLD,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT SN TABLE'
         GO TO 990
         END IF
C                                       create output
      OUTVER = 0
      CALL SNINI ('WRIT', BUFNEW, VOLNEW, CNONEW, OUTVER, CATNEW,
     *   LUNNEW, OSNRNO, SNKOLO, SNNUMO, NUMANT, NUMPOL, NIFO, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT SN TABLE'
         GO TO 990
         END IF
      NREC = BUFOLD(5)
C                                       read/write loop
      DO 50 IREC = 1,NREC
         ISNRNO = IREC
         CALL TABSN ('READ', BUFOLD, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT SN TABLE'
            GO TO 990
            END IF
         NAVG1 = 0
         NAVG2 = 0
         REF1 = MAXANT
         REF2 = MAXANT
         CALL RFILL (10, 0.0, DAVG)
         DO 20 I = 1,NUMIF
            IF (IF2AVG(I).GT.0) THEN
               IF ((CREAL(1,I).NE.FBLANK) .AND. (CIMAG(1,I).NE.FBLANK)
     *            .AND. (DELAY(1,I).NE.FBLANK) .AND.
     *            (RATE(1,I).NE.FBLANK) .AND. (WEIGHT(1,I).GT.0.0)) THEN
                  DAVG(1,1) = DAVG(1,1) + CREAL(1,I)
                  DAVG(1,2) = DAVG(1,2) + CIMAG(1,I)
                  DAVG(1,3) = DAVG(1,3) + DELAY(1,I)
                  DAVG(1,4) = DAVG(1,4) + RATE(1,I)
                  DAVG(1,5) = DAVG(1,5) + WEIGHT(1,I)
                  NAVG1 = NAVG1 + 1
                  IF (REFA(1,I).GT.0) REF1 = MIN (REF1, REFA(1,I))
                  END IF
               IF ((CREAL(2,I).NE.FBLANK) .AND. (CIMAG(2,I).NE.FBLANK)
     *            .AND. (DELAY(2,I).NE.FBLANK) .AND.
     *            (RATE(2,I).NE.FBLANK) .AND. (WEIGHT(2,I).GT.0.0)) THEN
                  DAVG(2,1) = DAVG(2,1) + CREAL(2,I)
                  DAVG(2,2) = DAVG(2,2) + CIMAG(2,I)
                  DAVG(2,3) = DAVG(2,3) + DELAY(2,I)
                  DAVG(2,4) = DAVG(2,4) + RATE(2,I)
                  DAVG(2,5) = DAVG(2,5) + WEIGHT(2,I)
                  NAVG2 = NAVG2 + 1
                  IF (REFA(2,I).GT.0) REF2 = MIN (REF2, REFA(2,I))
                  END IF
               END IF
 20         CONTINUE
         DO 30 I = 1,5
            IF (NAVG1.GT.0) THEN
               DAVG(1,I) = DAVG(1,I) / NAVG1
            ELSE
               REF1 = 0
               DAVG(1,I) = FBLANK
               END IF
            IF (NAVG2.GT.0) THEN
               DAVG(2,I) = DAVG(2,I) / NAVG2
            ELSE
               REF2 = 0
               DAVG(2,I) = FBLANK
               END IF
 30         CONTINUE
         DO 40 I = 1,NIFO
            CREAL(1,I) = DAVG(1,1)
            CIMAG(1,I) = DAVG(1,2)
            DELAY(1,I) = DAVG(1,3)
            RATE(1,I) = DAVG(1,4)
            WEIGHT(1,I) = DAVG(1,5)
            REFA(1,I) = REF1
            CREAL(2,I) = DAVG(2,1)
            CIMAG(2,I) = DAVG(2,2)
            DELAY(2,I) = DAVG(2,3)
            RATE(2,I) = DAVG(2,4)
            WEIGHT(2,I) = DAVG(2,5)
            REFA(2,I) = REF2
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
 1000 FORMAT ('SNCOPY ERROR',I4,' DOING ',A)
      END
