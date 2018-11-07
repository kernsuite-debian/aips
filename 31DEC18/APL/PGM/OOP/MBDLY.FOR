LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=14)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6          7      8      9           10          11
     *   'OUTVERS', 'BIF', 'EIF', 'TIMERANG', 'SUBARRAY', 'FREQID',
C           12        13       14
     *   'OPTYPE', 'PRTLEV', 'APARM'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7       8       9      10      11
     *   OOAINT, OOAINT, OOAINT, OOARE, OOAINT, OOAINT,
c          12      13      14
     *   OOACAR, OOAINT, OOARE/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
C         6    7    8    9    10   11
     *   1,1, 1,1, 1,1, 8,1, 1,1, 1,1,
C         12    13    14
     *   4,1,  1,1, 10,1 /
LOCAL END
      PROGRAM MBDLY
C-----------------------------------------------------------------------
C! Fits multiband delays from IF phases, updates SN table
C# Calibration VLBI OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2004-2006, 2009, 2012, 2015
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
C   Fits multiband delays from IF phases, updates SN table
C      April 2002: Does not change SN table phases.
C      June 2003:  Does not blank out 'bad' data (APARM(7) > 0);
C                  Puts bad quality code into Node Number column
C                     of SN table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, OUTTAB*36
      INTEGER   IRET, BUFF1(256)
      DATA PRGM /'MBDLY '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL MBDIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL MBDELY (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL MBDHI (OUTTAB)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE MBDIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   MBDIN gets input parameters for MBDLY and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INTAB*36, OUTTAB*36
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=13)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=5)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INTEGER   DIM(3), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6      7      8           9           10        11
     *   'BIF', 'EIF', 'TIMERANG', 'SUBARRAY', 'FREQID', 'PRTLEV',
C           12       13
     *   'APARM', 'OPTYPE'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6      7      8        9           10        11
     *   'BIF', 'EIF', 'TIMER', 'SUBARRAY', 'FREQID', 'PRTLEV',
C           12       13
     *   'APARM', 'OPTYPE'/
C                                       Adverbs to copy to OUTTAB
C                   1         2          3        4         5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTVERS'/
C                                       May rename adverbs to OUTTAB
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER'/
C-----------------------------------------------------------------------
C                                       Startup,  returns "Input" object
C                                       containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object
      INTAB = 'Input table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SN', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      CALL OPUT (OUTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SN', IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MBDHI (OUTTAB)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.  This assumes
C   that a previous history exists and merely adds the information from
C   the current task.
C   Inputs:
C      OUTTAB  C*?  Output table object
C-----------------------------------------------------------------------
      CHARACTER OUTTAB*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=6)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INVERS', 'OUTVERS',
     *   'APARM'/
C-----------------------------------------------------------------------
C                                       Add task label to history
      CALL OHTIME (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy adverb values.
      CALL OHLIST ('Input', LIST, NADV, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUTTAB
      CALL MSGWRT (4)
 999  RETURN
      END
      SUBROUTINE MBDELY (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Fit multiband delays, average rates and write corrected SN table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C      OUTTAB  C*   Name of output table object.
C   Inputs from INTAB:
C      APARM   R(10) Editing control paramaters
C                     (1) = min SNR
C                     (2) = max rate rms (sigma)
C                     (3) = max phase rms (sigma)
C                     (4) = min. no. IFs.
C                     (5) = set scan average phase = 0?
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER ANAME(MAXANT)*8, CDUMMY*1, OPTYPE*4
      INTEGER   IROW, OROW, NUMANT, NUMPOL, NUMIF, NUMNOD, I, SOURID,
     *   ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), ISBAND(MAXIF),
     *   TYPE, DIM(3), NROW, PRTLEV, MINIF, FLGMIF, FLGSNR, FLGRAS,
     *   FLGPHS, IP, IF, NTOTAL, NALL, BAD(2), FQID, ISUBA, BADIF(2),
     *   BADSNR(2), BADFAZ(2), BADRAT(2), PASBAD, MSGSAV, J
      LOGICAL   ISAPPL, SSF
      REAL      GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDEL(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), FINC(MAXIF), MINSNR, MAXRAS, MAXPHS, AVGPHS,
     *   DISP(2), DDISP(2), DISPRM(3)
      DOUBLE PRECISION TIME, FRQTAB(MAXIF), FRQTAZ(MAXIF), LAM, X,
     *   SUMX, SUMXX, SUML, SUMLX, TIMD, REFREQ, REFREZ, FACTM(MAXIF),
     *   FACTP(MAXIF)
      DOUBLE PRECISION TSTART, TSTOP
      INTEGER  NSOUR
      CHARACTER*8 SNAME(400), BNDCOD(MAXIF)
      CHARACTER*12  FNAME
      INTEGER   FDUMMY(3), BIF, EIF, NCHAN
      REAL      MAXMBD, REFP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Open input table
      CALL OSNINI (INTAB, 'READ', IROW, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output table
      CALL OSNINI (OUTTAB, 'WRIT', OROW, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get editing control parameters
      CALL GETEDT (INTAB, MINSNR, MAXRAS, MAXPHS, MAXMBD, AVGPHS, MINIF,
     *   PASBAD, DISPRM, TSTART, TSTOP, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1021) MINSNR
      CALL MSGWRT (4)
      WRITE (MSGTXT,1022) MAXRAS, MAXPHS
      CALL MSGWRT (4)
      WRITE (MSGTXT,1023) MAXMBD
      CALL MSGWRT (4)
C                                       get operation type
      CALL OGET (INTAB, 'OPTYPE', TYPE, DIM, PRTLEV, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get print level
      CALL OGET (INTAB, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get subarray number
      CALL OGET (INTAB, 'SUBARRAY', TYPE, DIM, ISUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ISUBA.LT.0) ISUBA = 0
C                                       Get freqid number
      CALL OGET (INTAB, 'FREQID', TYPE, DIM, FQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (FQID.LT.0) FQID = 0
C                                       Get frequency info
      CALL OCHNDA (INTAB, 'READ', NUMIF, FRQTAZ, ISBAND, FINC, BNDCOD,
     *   FQID, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get antenna info (esp. ref.
C                                       freq.)
      CALL ANTIFO (INTAB, ISUBA, ANAME, REFREZ, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do NOT depend on antenna table
      CALL OBHGET (INTAB, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVPGET (IERR)
      REFREZ = FREQ
      REFP = CATR(KRCRP+JLOCF)
      NCHAN = CATBLK(KINAX+JLOCF)
C                                       Get source names
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL SUINFO (INTAB, ISUBA, NSOUR, SNAME, SSF, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 999
C                                       For SSF, get file name
      IF (SSF) THEN
         CALL OGET (INTAB, 'NAME', TYPE, DIM, FDUMMY, FNAME, IERR)
         IF (IERR.NE.0) GO TO 999
         SNAME(1) = FNAME(1:8)
         END IF
C                                       Get IF's to use in fit
      CALL OGET (INTAB, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      CALL OGET (INTAB, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (BIF.LE.0) BIF = 1
      IF (EIF.LE.0) EIF = NUMIF
      I = EIF - BIF + 1
      IF ((MINIF.EQ.0) .OR. (MINIF.GT.I)) MINIF = I
      WRITE (MSGTXT,1030) BIF, EIF, MINIF
      CALL MSGWRT (4)
C                                       Determine Frequencies used
      REFREQ = REFREZ + FRQTAZ(BIF)
      WRITE (MSGTXT,1033) REFREQ / 1.0D6, FQID, ISUBA
      CALL MSGWRT (4)
      DO 50 I = BIF,EIF
         FRQTAB(I) = FRQTAZ(I) - FRQTAZ(BIF)
         WRITE (MSGTXT,1034) I, FRQTAB(I)/1.D6
         CALL MSGWRT (4)
         SUMX = 0.0D0
         SUMXX = 0.0D0
         SUML = 0.0D0
         SUMLX = 0.0D0
         DO 40 J = 1,NCHAN
            X = (J-REFP)*FINC(I)
            LAM = VELITE / (REFREQ + FRQTAB(I) + X)
            SUMX = SUMX + X
            SUMXX = SUMXX + X * X
            SUML = SUML + LAM
            SUMLX = SUMLX + LAM * X
 40         CONTINUE
         X = NCHAN * SUMXX - SUMX * SUMX
         FACTM(I) = VELITE * (NCHAN * SUMLX - SUMX * SUML) / X
         FACTP(I) = (SUMXX * SUML - SUMX * SUMLX) / X
 50      CONTINUE
C                                       Keep track of flagging
      FLGMIF = 0
      FLGSNR = 0
      FLGRAS = 0
      FLGPHS = 0
      NTOTAL = 0
      NALL   = 0
C                                       Process table
      DO 100 I = 1,NROW
         IROW = I
C                                       Read
         CALL OTABSN (INTAB, 'READ', IROW, NUMPOL, TIME, TIMEI, SOURID,
     *      ANTNO, SUBA, FREQID, IFR, NODENO, MBDEL, DISP, DDISP, CREAL,
     *      CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.NE.0) GO TO 999
C                                       Check time range
         IF (TIME.LT.TSTART) GO TO 100
         IF (TIME.GT.TSTOP) GO TO 900
         TIMD = TIME * 360.0D0
C                                       Check subarray
         IF ((SUBA.NE.ISUBA) .AND. (SUBA.NE.0).AND.
     *        (ISUBA.NE.0)) GO TO 100
C                                       Check FQID
         IF ((FREQID.NE.FQID) .AND. (FREQID.NE.0).AND.
     *       (FQID.NE.0)) GO TO 100
C                                       Average Rates and filter
         CALL AVRATE (NUMIF, MINIF, BIF, EIF, MINSNR, MAXRAS, ANTNO,
     *      REFA, NUMPOL, TIMEI, REFREQ, RATE, WEIGHT, BAD, BADIF,
     *      BADSNR, BADRAT, FLGMIF, FLGSNR, FLGRAS)
C                                       fit delay for dispersion
         IF (OPTYPE.EQ.'DISP') THEN
            CALL DELAMB (NUMIF, SSF, NUMPOL, FACTM, FACTP, CREAL, CIMAG,
     *         DELAY, WEIGHT, REFA, MBDEL, DISP, TIMD, ANTNO, RATE,
     *         DISPRM, NTOTAL, NALL, SOURID, SNAME)
C                                       Fit MB delays
         ELSE
            CALL GETMB (NUMIF, SSF, MAXPHS, MAXMBD, AVGPHS, NUMPOL,
     *         FRQTAB, CREAL, CIMAG, DELAY, WEIGHT, REFA, PRTLEV, MBDEL,
     *         BAD, BADFAZ, FLGPHS, TIMD, ANTNO, RATE, NTOTAL, NALL,
     *         SOURID, SNAME)
            END IF
C                                       Pass bad: Assign quality code + 10
         IF (PASBAD.GE.1) THEN
            IF (BADSNR(1).GE.1) NODENO = 17
            IF (BADRAT(1).GE.1) NODENO = 16
            IF (BADFAZ(1).GE.1) NODENO = 15
            IF (BADIF(1) .GE.1) NODENO = 10
C                                       If BAD flag MB delays
         ELSE
            IF (BAD(1).GT.0) MBDEL(1) = FBLANK
            IF (BAD(2).GT.0) MBDEL(2) = FBLANK
            IF (BAD(1).GT.0) DISP(1) = FBLANK
            IF (BAD(2).GT.0) DISP(2) = FBLANK
            IF (BAD(1).GT.0) DDISP(1) = FBLANK
            IF (BAD(2).GT.0) DDISP(2) = FBLANK
            DO 90 IP = 1,NUMPOL
               IF (MBDEL(IP).EQ.FBLANK) THEN
                  DO 80 IF = 1, NUMIF
                     CREAL(IP,IF) = FBLANK
                     CIMAG(IP,IF) = FBLANK
 80                  CONTINUE
                  END IF
 90            CONTINUE
            END IF
C                                       Write
         CALL OTABSN (OUTTAB, 'WRIT', OROW, NUMPOL, TIME, TIMEI, SOURID,
     *      ANTNO, SUBA, FREQID, IFR, NODENO, MBDEL, DISP, DDISP, CREAL,
     *      CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Close tables
 900  CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Processed fits
      WRITE (MSGTXT,1098) NALL
      CALL MSGWRT (4)
      WRITE (MSGTXT,1099) NTOTAL
      CALL MSGWRT (4)
C                                       Flagging summary
C                                       Min IF.
      IF (FLGMIF.GT.0) THEN
         WRITE (MSGTXT,1100) FLGMIF, MINIF
         CALL MSGWRT (4)
         END IF
C                                       Min. SNR
      IF (FLGSNR.GT.0) THEN
         WRITE (MSGTXT,1101) FLGSNR, MINSNR
         CALL MSGWRT (4)
         END IF
C                                       Max. rate scatter
      IF (FLGRAS.GT.0) THEN
         WRITE (MSGTXT,1102) FLGRAS, MAXRAS
         CALL MSGWRT (4)
         END IF
C                                       Max. phase scatter
      IF (FLGPHS.GT.0) THEN
         WRITE (MSGTXT,1103) FLGPHS, MAXPHS
         CALL MSGWRT (4)
         END IF
C                                       Phase centering
      IF (AVGPHS.EQ.0.0) THEN
         WRITE (MSGTXT,1104)
         CALL MSGWRT (4)
      ELSE
         WRITE (MSGTXT,1105)
         CALL MSGWRT (4)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1021 FORMAT ('SNRmin=', F7.1)
 1022 FORMAT ('Rate rms (deg) =',F6.1,'  MBD rms (deg) =', F6.1)
 1023 FORMAT ('MBD search range (nsec) = +/-', F7.1)
 1030 FORMAT ('Using IFs ', I2, ' to ', I2,'  Min IF allowed ', I2)
 1033 FORMAT (' Reference Frequency (MHz) ', F14.6,I6,I6)
 1034 FORMAT ('IF ', I3, '    Offset frequency ', F14.6)
 1098 FORMAT ('Total number of possible fits   = ', I5)
 1099 FORMAT ('Total number of acceptible fits = ', I5)
 1100 FORMAT ('Flagged ',I5,' soln. due <',I3,' IFs')
 1101 FORMAT ('Flagged ',I5,' soln. due to SNR <=',F12.2)
 1102 FORMAT ('Flagged ',I5,' soln. due to phase scatter from rate  >',
     *   F7.1,' deg')
 1103 FORMAT ('Flagged ',I5,' soln. due to phase scatter of MBD >',
     *   F7.1,' deg')
 1104 FORMAT ('Phase offset NOT removed from each scan')
 1105 FORMAT ('Phase offset removed from each scan')
      END
      SUBROUTINE GETEDT (INTAB, MINSNR, MAXRAS, MAXPHS, MAXMBD, AVGPHS,
     *   MINIF, PASBAD, DISPRM, TSTART, TSTOP, IERR)
C-----------------------------------------------------------------------
C   Gets editing control parameters, sets defaults and updates the
C   values in the input object.
C   Inputs:
C      INTAB   C*   Name of input table object.
C   Inputs from INTAB:
C      APARM   R(10) Editing control paramaters
C                     0 --> Do not test, except for IF's
C                     (1) = min SNR, default = 0
C                     (2) = max rate rms (sigma), default = 1.0E20
C                     (3) = max phase rms (sigma), default = 1.0E20
C                     (4) = min. no. IFs., default = all
C                     (5) = MBD search range. 0 = ambiguity
C                     (6) = residual phase flag
C   Output:
C      MINSNR  R    Min SNR
C      MAXRAS  R    Max rate rms (sigma)
C      MAXPHS  R    Max phase rms (sigma)
C      MAXMBD  R    MBD search range (nsec)
C      AVGPHS  R    0.0 = Keep phase offset
C      MINIF   I    Min. no. IFs.
C      PASBAD  I    Pass "bad" data if >= 1
C      TSTART  D    Start time in days
C      TSTOP   D    Stop time in days
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      DOUBLE PRECISION   TSTART, TSTOP
      REAL      MINSNR, MAXRAS, MAXPHS, MAXMBD, AVGPHS, DISPRM(3)
      INTEGER   MINIF, PASBAD, IERR
C
      INTEGER   TYPE, DIM(3)
      REAL      APARM(10), TIMER(8)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Get APARM array
      CALL OGET (INTAB, 'APARM', TYPE, DIM, APARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set defaults: min SNR
      IF (APARM(1).LE.1.0E-10) APARM(1) = 5.0
C                                       Max rate rms
      IF (APARM(2).LE.1.0E-10) APARM(2) = 50.0
C                                       Max phase rms
      IF (APARM(3).LE.1.0E-10) APARM(3) = 20.0
C                                       MBD Search Range
C                                       Pass bad data?
      PASBAD = APARM(7) + 0.001
      IF (APARM(7).GT.0.0) PASBAD = MAX (1, PASBAD)
C                                       Save for history
      CALL OPUT ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set output values
      MINSNR = APARM(1)
      MAXRAS = APARM(2)
      MAXPHS = APARM(3)
      MINIF = APARM(4) + 0.5
      MAXMBD = APARM(5)
      IF (MAXMBD.LT.0.0) THEN
         MAXMBD = 1.0E-20
         MINSNR = 0.001
         MAXRAS = 1000.0
         MAXPHS = 1000.0
         END IF
      AVGPHS = APARM(6)
      DISPRM(1) = APARM(8)
      DISPRM(2) = APARM(9)
      DISPRM(3) = APARM(10)
C                                       Get TIMER array
      CALL OGET (INTAB, 'TIMER', TYPE, DIM, TIMER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      TSTART = TIMER(1) + TIMER(2) / 24.0D0 + TIMER(3) / 1440.0D0 +
     *         TIMER(4) / 86400.0D0
      TSTOP  = TIMER(5) + TIMER(6) / 24.0D0 + TIMER(7) / 1440.0D0 +
     *         TIMER(8) / 86400.0D0
      WRITE (MSGTXT, 1020) INT(TIMER(1)), INT(TIMER(2)), INT(TIMER(3)),
     *                     INT(TIMER(4)), INT(TIMER(5)), INT(TIMER(6)),
     *                     INT(TIMER(7)), INT(TIMER(8))
      CALL MSGWRT (4)
      IF (TSTART .EQ. 0.0D0) TSTART=-100.0D0
      IF (TSTOP  .EQ. 0.0D0) TSTOP = 1000.0D0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Time range: ',I2,'/',3I3.2,' to ',I2,'/',3I3.2)
      END
      SUBROUTINE AVRATE (NIF, MINIF, BIF, EIF, MINSNR, MAXRAS, ANTNO,
     *   REFA, NPOLN, TIMEI, FREQ, RATE, WEIGHT, BAD, BADIF, BADSNR,
     *   BADRAT, FLGMIF, FLGSNR, FLGRAS)
C-----------------------------------------------------------------------
C   Averages valid rates and applies a number of editing criteria.
C   Inputs:
C      NIF     I    Number of IFs.
C      MINIF   I    Min. no. IFs.
C      BIF     I    First IF to use in fit.
C      EIF     I    Last IF to use in fit.
C      MINSNR  R    Min SNR
C      MAXRAS  R    Max rate rms (sigma)
C      NPOLN   I    Number of polarizations (1 or 2)
C      ANTNO   I    Antenna number, (min SNR not applied to ref.
C                   antenna).
C      REFA    I(2,*) Reference antenna
C      TIMEI   R      Time interval of record (days)
C      FREQ    D      Reference frequency (Hz)
C      WEIGHT  R(2,*) SNR of each IF by poln. Used for validity.
C   Input/Output:
C      RATE    R(2,*) Fringe rates (s/s)
C      BAD     I(2) If data, 1 per poln.
C      FLGMIF  I    Count of records flagged for too few IFs
C      FLGSNR  I    Count of records flagged for too low SNR
C      FLGRAS  I    Count of records flagged for excessive rate scatter.
C-----------------------------------------------------------------------
      INTEGER   NIF, MINIF, ANTNO, REFA(2,*), NPOLN, FLGMIF, FLGSNR,
     *   FLGRAS, BIF, EIF, BAD(2), BADIF(2), BADSNR(2), BADRAT(2)
      REAL      MINSNR, MAXRAS, TIMEI, RATE(2,*), WEIGHT(2,*)
      DOUBLE PRECISION FREQ
C
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   IIF, COUNT, IPOL, CNTSNR, IREF, GETREF, MIF(2)
      REAL      SUM, SUM2, SUMSNR, SNRSUM, SIGRA, ERRRA
C-----------------------------------------------------------------------
C                                       Average
      SUM = 0.0
      SNRSUM = 0.0
      SUM2 = 0.0
      COUNT = 0
      MIF(1) = 0
      MIF(2) = 0
      BAD(1) = 0
      BAD(2) = 0
      BADIF(1) = 0
      BADIF(2) = 0
      BADSNR(1) = 0
      BADSNR(2) = 0
      BADRAT(1) = 0
      BADRAT(2) = 0
C                                       Loop over polarization
      DO 150 IPOL = 1,NPOLN
         CNTSNR = 0
         SUMSNR = 0.0
         DO 100 IIF = 1,NIF
C                                       Only use BIF to EIF
            IF (IIF .LT. BIF .OR. IIF. GT. EIF)
     *         WEIGHT(IPOL,IIF) = 0.0

            IF ((WEIGHT(IPOL,IIF).GT.0.0001) .AND.
     *         (RATE(IPOL,IIF).NE.FBLANK)) THEN
               MIF(IPOL) = MIF(IPOL) + 1
               COUNT = COUNT + 1
               CNTSNR = CNTSNR + 1
               SUM = SUM + RATE(IPOL,IIF)
               SUM2 = SUM2 + RATE(IPOL,IIF)**2
               SUMSNR = SUMSNR + (WEIGHT(IPOL,IIF))**2
               SNRSUM = SNRSUM + (WEIGHT(IPOL,IIF))**2
               END IF
 100        CONTINUE
         SUMSNR = SQRT(SUMSNR)
C                                       IF editing:
C                                       Must have enough IFs present
         IF (MIF(IPOL).LT.MINIF) THEN
            BAD(IPOL) = BAD(IPOL) + 1
            BADIF(IPOL) = 1
            FLGMIF = FLGMIF + 1
            END IF
C                                       IF dependent SNR test, only if
C                                       not refant.
         IREF = GETREF (NIF, IPOL, REFA, RATE, WEIGHT)
         IF (ANTNO.NE.IREF) THEN
            IF (CNTSNR.GT.1) THEN
               IF (SUMSNR .LT. MINSNR) THEN
                  BAD(IPOL) = BAD(IPOL) + 2
                  BADSNR(IPOL) = 1
                  FLGSNR = FLGSNR + 1
                  END IF
            ELSE
               BAD(IPOL) = BAD(IPOL) + 2
               FLGSNR = FLGSNR + 1
               END IF
            END IF
 150     CONTINUE
C                                       Use both IFs (if present to look
C                                       for bad rates)
      IF (COUNT.GT.1) THEN
         SIGRA = SQRT ((SUM2 - (SUM*SUM / COUNT)) / (COUNT-1))
C                                       Phase scatter from rate scat.
         ERRRA = SIGRA * TIMEI * 86400.0 * FREQ * 57.25
         IF (ERRRA .GT. MAXRAS) THEN
            BAD(1) = BAD(1) + 4
            BAD(2) = BAD(2) + 4
            BADRAT(1) = 1
            BADRAT(2) = 1
            FLGRAS = FLGRAS + NPOLN
            END IF
         END IF
C                                       Average rate
      IF (COUNT.GT.0) SUM = SUM / COUNT
C                                       Correct
      DO 250 IPOL = 1,NPOLN
         DO 200 IIF = 1,NIF
            IF ((WEIGHT(IPOL,IIF).GT.0.0001) .AND.
     *         (RATE(IPOL,IIF).NE.FBLANK)) RATE(IPOL,IIF) = SUM
 200        CONTINUE
 250     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DELAMB (NIF, SSF, NPOLN, FACTM, FACTP, CREAL, CIMAG,
     *   DELAY, WEIGHT, REFA, MBDLY, DISP, TIMD, ANTNO, RATE, DISPRM,
     *   NTOTAL, NALL, SOURID, SNAME)
C-----------------------------------------------------------------------
C   Determine multiband delays from IF phases and single band delays.
C   Also corrects phases.
C   Inputs:
C      NIF      I        Number of IFs
C      SSF      L        Single source file
C      NPOLN    I        Number of polarizations (1 or 2)
C      WEIGHT   R(2,*)   SNR of each IF by poln. (1/var. of phase in
C                        rad.)
C      REFA     I(2,*)   Ref ant. of each IF by poln.
C      TIMD     D        Time of data
C      ANTNO    I        Antenna Number
C      RATE     R(2,*)   Rate solutions
C      DISPRM   R(2)     (1) > 0 -> zero phase, delay, rate
C                        (2) <= 0 -> correct phase, delay for dispersion
C                        (3) > 0 -> average 2 polarizations dispersion
C      SOURID   I        Source Number
C      SNAME    C(*)8    Source list
C   Input/Output:
C      CREAL    R(2,*)   Real part of phase of both polarizations
C      CIMAG    R(2,*)   Imaginary part of phase of  both polarizations
C      DELAY    R(2,*)   Single band delay of both polarizations
C   Outputs:
C      MBDLY    R(2)     Multi band delay of both polarizations.
C      DISP     R(2)     Dispersion
C      NTOTAL   I        Total number of good fits
C      NALL     I        Total number of possible fits
C-----------------------------------------------------------------------
      INTEGER   NIF, NPOLN, REFA(2,*), ANTNO, NTOTAL, NALL, SOURID
      LOGICAL   SSF
      REAL      CREAL(2,*), CIMAG(2,*), DELAY(2,*), WEIGHT(2,*),
     *   MBDLY(2), DISP(2), RATE(2,*), DISPRM(3)
      DOUBLE PRECISION TIMD, FACTM(*), FACTP(*)
      CHARACTER SNAME(*)*8
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION TIMPR
      CHARACTER CHM*1
      LOGICAL   PTITLE
      INTEGER   LOOP, ISUM, IPOL, IREF, GETREF, HM(2), ISNR(2)
      REAL      WT(MAXIF), DAVG(2), MBDLE(2), A, P, SNR, SUMSNR, SEC,
     *   FRATE, DISPE(2), DELY(MAXIF)
      INCLUDE 'INCS:PSTD.INC'
      SAVE TIMPR, PTITLE
      DATA  TIMPR / 0.0D0 /
      DATA  PTITLE /.FALSE./
C-----------------------------------------------------------------------
C                                       Print header
      IF (TIMPR.NE.TIMD) THEN
         TIMPR = TIMD
         PTITLE = .TRUE.
         END IF
C                                       Loop over polarization
      DO 40 IPOL = 1,NPOLN
         NALL = NALL + 1
C                                       This one any good?
         MBDLY(IPOL) = FBLANK
         DISP(IPOL) = FBLANK
C                                       Most common REFANT.
         IREF = GETREF (NIF, IPOL, REFA, CREAL, WEIGHT)
C                                       Must be ref. antenna, valid data
         IF (IREF.LE.0) GO TO 40
         DAVG(IPOL) = 0.0
         SUMSNR = 0.0
         ISUM = 0
C                                       Form part of first line
         CALL COORDD (1, TIMD, CHM, HM, SEC)
         FRATE = RATE(IPOL, 1) * 1.0E12
C                                       Convert to ampl. phase.; average
C                                       SNR.
         DO 30 LOOP = 1,NIF
            IF ((CREAL(IPOL,LOOP).NE.FBLANK) .AND.
     *         (CIMAG(IPOL,LOOP).NE.FBLANK) .AND.
     *         (DELAY(IPOL,LOOP).NE.FBLANK) .AND.
     *         (WEIGHT(IPOL,LOOP).GT.0.0) .AND.
     *         (REFA(IPOL,LOOP) .EQ. IREF))  THEN
               WT(LOOP) = WEIGHT(IPOL,LOOP)
               ISUM = ISUM + 1
C                                       Get average SB delay
               DELY(LOOP) = DELAY(IPOL,LOOP)
               DAVG(IPOL) = DAVG(IPOL) + DELAY(IPOL,LOOP)
C                                       Sum SNR
               SUMSNR = SUMSNR + WT(LOOP)
            ELSE
               DELY(LOOP) = 0.0
               WT(LOOP) = 0.0
               END IF
 30         CONTINUE
C                                       Less than 2 IF's?
         IF (ISUM.LE.1) GO TO 40
C                                       Poln. SNR average
         SNR = SQRT (SUMSNR)
         SNR = MAX (0.01, SNR)
         ISNR(IPOL) = SNR + 0.5
C                                       Determine MB delay, disp
         DAVG(IPOL) = DAVG(IPOL) / ISUM
         CALL DELFIT (NIF, DELY, WT, FACTM, MBDLY(IPOL), MBDLE(IPOL),
     *      DISP(IPOL), DISPE(IPOL))
         NTOTAL = NTOTAL + 1
 40      CONTINUE
C                                       Correct phases
      IF ((DISPRM(3).GT.0.0) .AND. (NPOLN.EQ.2)) THEN
         IF ((DISP(1).NE.FBLANK) .AND. (DISP(2).NE.FBLANK)) THEN
            DISP(1) = (DISP(1) + DISP(2)) / 2.0
            DISP(2) = DISP(1)
            END IF
         END IF
      DO 60 IPOL = 1,NPOLN
         IF (DISP(IPOL).NE.FBLANK) THEN
            DISP(IPOL) = -DISP(IPOL)
C                                       Don't write reference ant
            IF (ANTNO.NE.REFA(1,1)) THEN
C                                       Write Heading
               IF (PTITLE) THEN
                  WRITE (MSGTXT,1002)
                  CALL MSGWRT (4)
                  IF (SSF) THEN
                     WRITE (MSGTXT, 1013) SNAME(1), CHM, HM, SEC,
     *                  REFA(1,1)
                     CALL MSGWRT (4)
                  ELSE
                     WRITE (MSGTXT, 1014) SNAME(SOURID), CHM, HM, SEC,
     *                  REFA(1,1)
                     CALL MSGWRT (4)
                     END IF
                  WRITE (MSGTXT,1004)
                  CALL MSGWRT (4)
                  WRITE (MSGTXT,1006)
                  CALL MSGWRT (4)
                  PTITLE = .FALSE.
                  END IF
C                                       Write solution
               WRITE (MSGTXT,1025) ANTNO, FRATE, DAVG(IPOL)*1.0E9,
     *            MBDLY(IPOL)*1.E9, MBDLE(IPOL)*1.E9, DISP(IPOL)*1.E9,
     *            DISPE(IPOL)*1.E9, ISNR(IPOL)
               CALL MSGWRT (4)
               END IF
            DO 50 LOOP = 1,NIF
               IF ((CREAL(IPOL,LOOP).NE.FBLANK) .AND.
     *            (CIMAG(IPOL,LOOP).NE.FBLANK)) THEN
C                                       null them
                  IF (DISPRM(1).GT.0.0) THEN
                     CREAL(IPOL,LOOP) = 1.0
                     CIMAG(IPOL,LOOP) = 0.0
                     DELAY(IPOL,LOOP) = 0.0
C                                       correct them
                  ELSE IF (DISPRM(2).LE.0.0) THEN
                     DELAY(IPOL,LOOP) = DELAY(IPOL,LOOP) -
     *                  FACTM(LOOP) * DISP(IPOL)
                     A = SQRT (CREAL(IPOL,LOOP)**2 +
     *                  CIMAG(IPOL,LOOP)**2)
                     P = ATAN2 (CIMAG(IPOL,LOOP), CREAL(IPOL,LOOP))
                     P = P - TWOPI * VELITE * FACTP(LOOP) * DISP(IPOL)
                     CREAL(IPOL,LOOP) = A * COS (P)
                     CIMAG(IPOL,LOOP) = A * SIN (P)
C                                       don't modify phases from FRING
C                                       they are needed in CL2HF
C                 ELSE
C                    CREAL(IPOL,LOOP) = COS (PHASE(LOOP))
C                    CIMAG(IPOL,LOOP) = SIN (PHASE(LOOP))
                     END IF
                  END IF
 50            CONTINUE
            END IF
 60      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT (20(' - '))
 1004 FORMAT ('Ant   Rate    <SBD>      MBD            DISP         ',
     *   'SNR')
 1006 FORMAT ('      ps/s      ns        ns          ns/m/m')
 1013 FORMAT ('File=',A8, 2X,'Time=',A1, 2I3, F6.2,'   Ref Ant= ',I2)
 1014 FORMAT ('Source=',A8, 2X,'Time=',A1, 2I3, F6.2,'   Ref Ant= ',I2)
 1025 FORMAT (I3,F7.2,F8.2,F10.4,'(',F6.4,')',F8.3,'(',F5.3,')',I5)
      END
      SUBROUTINE DELFIT (NUMIF, DELY, PHASWT, FACTM, MB, MBE, DISP,
     *   DISPE)
C-----------------------------------------------------------------------
C   Fit Single-band delays (i) to MBdelay + FACTM(i) * Dispersion
C   Inputs:
C      NUMIF     I      Number of IF.
C      DELY      R(*)   delay for IF (s).
C      PHASWT    R(*)   Weight for IF. (non positive means bad)
C      FACTM     D(*)   LSQ factor dispersion addition to delay
C   Output:
C      MB      R    Multi band delay (seconds)
C      MBE     R    Multi band delay error (seconds)
C      DISP    R    Dispersion (seconds/m/m)
C      DISPE   R    Dispersion error (seconds/m/m)
C-----------------------------------------------------------------------
      INTEGER   NUMIF
      REAL      DELY(NUMIF), PHASWT(NUMIF), MB, MBE, DISP, DISPE
      DOUBLE PRECISION FACTM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOP, COUNT
      DOUBLE PRECISION SUMX, SUMY, SUMXX, SUMXY, DIV, DTEMP1, DTEMP2,
     *   LL(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Find first good IF
      MB = FBLANK
      DISP = FBLANK
C                                       Init sums
      COUNT = 0
      SUMX  = 0.0
      SUMXX = 0.0
      SUMY  = 0.0
      SUMXY = 0.0
C                                       Least squares slope
      DO 130 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).GT.0.0) THEN
            COUNT = COUNT + 1
            LL(LOOP) = FACTM(LOOP)
            SUMX  = SUMX + LL(LOOP)
            SUMXX = SUMXX + LL(LOOP) * LL(LOOP)
            SUMY  = SUMY + DELY(LOOP)
            SUMXY = SUMXY + DELY(LOOP) * LL(LOOP)
            END IF
 130     CONTINUE
C                                       Slope
      IF (COUNT.LE.0) GO TO 999
      DIV = COUNT * SUMXX - SUMX * SUMX
      IF (DIV.EQ.0.0) GO TO 999
      MB = (SUMY * SUMXX - SUMX * SUMXY) / DIV
      DISP = (SUMY * SUMX - COUNT * SUMXY) / DIV
C                                       MB uncertainty done right
      MBE = 0.0
      DISPE = 0.0
      DTEMP2 = 0.0D0
      DO 140 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).GT.0.0) THEN
            DTEMP1 = (SUMXX - SUMX * LL(LOOP)) / DIV
            MBE = MBE + DTEMP1*DTEMP1
            DTEMP1 = (SUMX - COUNT * LL(LOOP)) / DIV
            DISPE = DISPE + DTEMP1*DTEMP1
            DTEMP2 = DTEMP2 + (DELY(LOOP)- MB + DISP*LL(LOOP)) ** 2
            END IF
 140     CONTINUE
      DTEMP2 = SQRT (DTEMP2/COUNT)
      MBE = SQRT (MBE) * DTEMP2
      DISPE = SQRT (DISPE) * DTEMP2
C
 999  RETURN
      END
      SUBROUTINE GETMB (NIF, SSF, MAXPHS, MAXMBD, AVGPHS, NPOLN, FRQTAB,
     *   CREAL, CIMAG, DELAY, WEIGHT, REFA, PRTLEV, MBDLY, BAD, BADFAZ,
     *   FLGPHS, TIMD, ANTNO, RATE, NTOTAL, NALL, SOURID, SNAME)
C-----------------------------------------------------------------------
C   Determine multiband delays from IF phases and single band delays.
C   Also corrects phases.
C   Inputs:
C      NIF    I      Number of IFs
C      SSF    L      Single source file
C      MAXPHS R      Maximum postfit phase RMS in units of the phase
C                    uncertainty.
C      MAXMBD R      Search range of MBD
C      AVGPHS R      Scan phase centering?
C      NPOLN  I      Number of polarizations (1 or 2)
C      FRQTAB D(*)   Frequency offset for each IF from reference freq.
C      DELAY  R(2,*) Single band delay of both polarizations
C      WEIGHT R(2,*) SNR of each IF by poln. (1/var. of phase in rad.)
C      REFA   I(2,*) Ref ant. of each IF by poln.
C      PRTLEV I      Print level, 1=MB result, 2=also phase residuals
C      TIMD   D      Time of data
C      ANTNO  I      Antenna Number
C      RATE   R(2,*) Rate solutions
C      SOURID I      Source Number
C      SNAME  C(*)8  Source list
C   Input/Output:
C      CREAL  R(2,*) Real part of phase of both polarizations
C      CIMAG  R(2,*) Imaginary part of phase of  both polarizations
C      BAD    I      Bad parameter  0 okay.
C      FLGPHS I      Count of records flagged for excessive post fit
C                    phase scatter.
C   Outputs:
C      MBDLY  R(2)   Multi band delay of both polarizations.
C      NTOTAL I      Total number of good fits
C      NALL   I      Total number of possible fits
C-----------------------------------------------------------------------
      INTEGER   NIF, NPOLN, REFA(2,*), PRTLEV, BAD(2), BADFAZ(2),
     *   FLGPHS, ANTNO, NTOTAL, NALL, SOURID
      LOGICAL   SSF
      REAL      MAXPHS, MAXMBD, AVGPHS, CREAL(2,*), CIMAG(2,*),
     *   DELAY(2,*), WEIGHT(2,*), MBDLY(2), RATE(2,*)
      DOUBLE PRECISION FRQTAB(*), TIMD
      CHARACTER SNAME(*)*8
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   ISNR, HM(2), II, LOOP, ISUM, IPOL, IREF, GETREF
      REAL      SEC, FRATE, DTEMP, RPHASE(MAXIF), AMP(MAXIF),
     *   PHASE(MAXIF), WT(MAXIF), DAVG, MBDLE(2),DLAMB(2), AVGFAZ(2),
     *   SNR, SUMSNR, RMSFAZ
      DOUBLE PRECISION TIMPR
      CHARACTER  CHM*1, MSGPHS(8)*4
      LOGICAL   PTITLE
      INCLUDE 'INCS:PSTD.INC'
      SAVE TIMPR, PTITLE
      DATA  TIMPR / 0.0D0 /
      DATA  PTITLE /.FALSE./
C-----------------------------------------------------------------------
       BADFAZ(1) = 0
       BADFAZ(2) = 0
C                                       Print header
      IF (TIMPR.NE.TIMD) THEN
         TIMPR = TIMD
         PTITLE = .TRUE.
         END IF
C                                       Loop over polarization
      DO 200 IPOL = 1,NPOLN
         NALL = NALL + 1
C                                       This one any good?
         MBDLY(IPOL) = FBLANK
C                                       Most common REFANT.
         IREF = GETREF (NIF, IPOL, REFA, CREAL, WEIGHT)
C                                       Must be ref. antenna, valid data
         IF (IREF.LE.0) GO TO 200
         DAVG = 0.0
         SUMSNR = 0.0
         ISUM = 0
C                                       Form part of first line
         CALL COORDD (1, TIMD, CHM, HM, SEC)
         FRATE = RATE(IPOL, 1) * 1.0E12
C                                       Convert to ampl. phase.; average
C                                       SNR.
         DO 30 LOOP = 1,NIF
            IF ((CREAL(IPOL,LOOP).NE.FBLANK) .AND.
     *         (CIMAG(IPOL,LOOP).NE.FBLANK) .AND.
     *         (DELAY(IPOL,LOOP).NE.FBLANK) .AND.
     *         (WEIGHT(IPOL,LOOP).GT.0.0) .AND.
     *         (REFA(IPOL,LOOP) .EQ. IREF))  THEN
               AMP(LOOP) = SQRT (CREAL(IPOL,LOOP)*CREAL(IPOL,LOOP) +
     *            CIMAG(IPOL,LOOP)*CIMAG(IPOL,LOOP))
               PHASE(LOOP) = ATAN2 (CIMAG(IPOL,LOOP),
     *            CREAL(IPOL,LOOP)+1.0E-20)
               WT(LOOP) = WEIGHT(IPOL,LOOP)
               ISUM = ISUM + 1
C                                       Get average SB delay
               DAVG = DAVG + DELAY(IPOL,LOOP)
C                                       Sum SNR
               SUMSNR = SUMSNR + WT(LOOP)**2
            ELSE
               AMP(LOOP) = 0.0
               PHASE(LOOP) = 0.0
               WT(LOOP) = 0.0
               END IF
 30         CONTINUE
C                                       Less than 2 IF's?
         IF (ISUM .LE. 1) GO TO 200
C                                       Poln. SNR average
         SNR = SQRT(SUMSNR)
         SNR = MAX (0.01, SNR)
         ISNR = SNR + 0.5
C                                       Determine MB delay
C                                       phases
         DAVG = DAVG / ISUM
         CALL MBFIT (NIF, MAXMBD, PHASE, WT, FRQTAB, DAVG, PRTLEV,
     *      MBDLY(IPOL), MBDLE(IPOL), DLAMB(IPOL), AVGFAZ(IPOL), RMSFAZ,
     *      RPHASE)
C                                       Excessive MBD phase scatter
         IF (RMSFAZ*RAD2DG .GT. MAXPHS) THEN
            BAD(IPOL) = 8 + BAD(IPOL)
            BADFAZ(IPOL) = 1
            FLGPHS = FLGPHS + 1
            END IF
C                                       Failed?
         IF (BAD(IPOL) .GT. 0) GO TO 49
         NTOTAL = NTOTAL + 1
C                                       Don't write reference ant
         IF (ANTNO .EQ. REFA(1,1)) GO TO 49
C                                       Write Heading
         IF (PTITLE) THEN
            WRITE (MSGTXT,1002)
            CALL MSGWRT (4)
            IF (SSF) THEN
               WRITE (MSGTXT, 1013) SNAME(1), CHM, HM, SEC,
     *            REFA(1,1)
               CALL MSGWRT (4)
            ELSE
               WRITE (MSGTXT, 1014) SNAME(SOURID), CHM, HM, SEC,
     *            REFA(1,1)
               CALL MSGWRT (4)
               END IF
            WRITE (MSGTXT, 1004)
            CALL MSGWRT (4)
            WRITE (MSGTXT,1006)
            CALL MSGWRT (4)
            PTITLE = .FALSE.
            END IF
C                                       Write solution
         IF (MAXMBD .NE. 0.0) THEN
            DTEMP = MAXMBD
         ELSE
            DTEMP = DLAMB(IPOL)*1.0E9
            END IF
         WRITE (MSGTXT, 1025) ANTNO, FRATE,
     *      DAVG*1.0E9,AVGFAZ(IPOL)*RAD2DG,
     *      RMSFAZ*RAD2DG, DTEMP, MBDLY(IPOL)*1.0E9,
     *      MBDLE(IPOL)*1.0E9, ISNR
         CALL MSGWRT (4)
         IF (PRTLEV.GE.2) THEN
            DO 47 II = 1,NIF
               WRITE (MSGPHS(II), 1026) NINT(RPHASE(II))
 47            CONTINUE
            MSGTXT = '                    Res: '//
     *         MSGPHS(1)//MSGPHS(2)//MSGPHS(3)//MSGPHS(4)//
     *         MSGPHS(5)//MSGPHS(6)//MSGPHS(7)//MSGPHS(8)
               CALL MSGWRT (4)
            DO 48 II = 1,NIF
               MSGPHS(II) = ' '
 48            CONTINUE
            END IF
 49      CONTINUE
C                                       Correct phases
         DO 50 LOOP = 1,NIF
            IF (AVGPHS.GT.0.0) THEN
               PHASE(LOOP) = -AVGFAZ(IPOL) + MBDLY(IPOL) *
     *            FRQTAB(LOOP) * TWOPI
            ELSE
               PHASE(LOOP) = MBDLY(IPOL) * FRQTAB(LOOP) * TWOPI
               END IF
C                                       don't modify phases from FRING
C                                       they are needed in CL2HF
C           IF ((CREAL(IPOL,LOOP).NE.FBLANK) .AND.
C    *         (CIMAG(IPOL,LOOP).NE.FBLANK)) THEN
C              CREAL(IPOL,LOOP) = COS (PHASE(LOOP))
C              CIMAG(IPOL,LOOP) = SIN (PHASE(LOOP))
C              END IF
 50         CONTINUE
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT (20(' - '))
 1004 FORMAT (' Ant  Rate   SBD     Phase   ',
     *   'Range        MBD    SNR')
 1006 FORMAT ('      ps/s   ns   zero  rms  ','  ns          ns')
 1013 FORMAT ('File=',A8, 2X,'Time=',A1, 2I3, F6.2,'   Ref Ant= ',I2)
 1014 FORMAT ('Source=',A8, 2X,'Time=',A1, 2I3, F6.2,'   Ref Ant= ',I2)
 1025 FORMAT (I3,F7.2,F6.1, F6.1, F5.1, F7.1, F8.3,'(', F5.3,')',I3)
 1026 FORMAT (I4)
      END
      SUBROUTINE MBFIT (NUMIF, MAXMBD, PHASE, PHASWT, FREQ, SB, PRTLEV,
     *   MB, MBE, DAMB, AVGFAZ, RMSFAZ, RPHASE)
C-----------------------------------------------------------------------
C   Fit phase slope (like multiband delay) to a set of phases.  This
C   routine is not particularly smart about resolving ambiguities.
C   Multiband results are constrained to be within 1/2 the ambiguity
C   spacing of 0.
C   Inputs:
C      NUMIF   I    Number of IF.
C      MAXMBD  R    Search range for MBD
C      PHASWT  R(*) Weight for IF. (non positive means bad)
C      FREQ    D(*) Frequencies (Hz) of IFs.
C      SB      R    The average SBDelay to center the MB
C      PRTLEV  I    Print level, 1=MB result, 2=also phase residuals
C      PHASE   R(*) Phase for IF (rad).
C   Output:
C      MB      R    Multi band delay (seconds)
C      MBE     R    Multi band delay error (seconds)
C      DAMB    R    Multi band delay ambiguity (seconds)
C      AVGFAZ  R    Average phase at ref. freq. (rad)
C      RMSFAZ  R    RMS phase residual. (rad)
C      RPHASE  R    Residial phase (deg) for each IF
C-----------------------------------------------------------------------
      INTEGER   NUMIF, PRTLEV
      REAL      PHASE(NUMIF), PHASWT(NUMIF), MB, SB, MBE, DAMB, AVGFAZ,
     *   RMSFAZ, RPHASE(NUMIF), MAXMBD
      DOUBLE PRECISION FREQ(NUMIF)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER LOOP, COUNT, GOOD1, LOOP1, FLAG(MAXIF),
     *   NSTEP, IMAX, ISUM, ILAST
      DOUBLE PRECISION P(MAXIF), SUMX, SUMY, SUMXX, SUMXY, DIV, FMIN,
     *   FMAX, DSTEP, DMAX, DVAL, DSUM, PREF, FREF, PTMP, DTEMP1,
     *   DTEMP2, DTEMP3, DLYMAX, GFREQ(MAXIF), DAMBA, XFREQ(MAXIF)
      DOUBLE PRECISION  IFINC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Find first good IF
      MB = FBLANK
      DAMB = FBLANK
      AVGFAZ = FBLANK
      GOOD1 = 0
C                                       Set IF flags
      DO 50 LOOP = 1,NUMIF
         GFREQ(LOOP) = FREQ(LOOP)
         FLAG(LOOP) = 0
         IF (PHASWT(LOOP).GT.0.0) THEN
            GOOD1 = GOOD1 + 1
            XFREQ(GOOD1) = GFREQ(LOOP)
            FLAG(LOOP) = 1
            END IF
 50      CONTINUE
C                                       Must have at least 2 IFs.
      IF (GOOD1.LE.1) GO TO 999
C                                       Find maximum freq. diff
      FMAX = 0.0D0
      DO 70 LOOP = 1, GOOD1
         DO 60 LOOP1 = LOOP, GOOD1
            FMAX = DMAX1(FMAX, DABS(XFREQ(LOOP)-XFREQ(LOOP1)))
 60         CONTINUE
 70      CONTINUE
C                                       Find minimum delay interval
      FMIN = IFINC (XFREQ, GOOD1)
C                                       Set up step for guess
      FMIN = MAX (FMIN, 1.0D-20)
      FMAX = MAX (FMAX, 1.0D-20)
      IF (MAXMBD .EQ. 0.0) THEN
         DAMB = 1.0 / FMIN
        ELSE
         DAMB = 2.0 * MAXMBD * 1.0E-9
         END IF
      DSTEP = 1.0 / FMAX / 36.0
      NSTEP = DAMB / DSTEP
      IF (NSTEP.GT.5000) THEN
         NSTEP = 5000
         DSTEP = DAMB / NSTEP
         END IF
      IF (NSTEP.LT.10) THEN
         NSTEP = 10
         DSTEP = DAMB / NSTEP
         END IF
C                                       Begin loop to get max coher
      DMAX = 0.0
      IMAX = 0
      DVAL = (-DAMB / 2.0) * TWOPI
      DO 100 LOOP = 1, NSTEP
         DVAL = DVAL + DSTEP * TWOPI
         DSUM = 0.0
         ISUM = 0
         DO 90 LOOP1 = 1,NUMIF
            IF (FLAG(LOOP1).GT.0) THEN
               ISUM = ISUM + 1
C                                       Reference Frequency
               IF (ISUM.EQ.1) THEN
                  DSUM = 1.0
                  PREF = PHASE(LOOP1)
                  FREF = GFREQ(LOOP1)
               ELSE
                  DSUM = DSUM + COS (PHASE(LOOP1) - PREF
     *               - DVAL * (GFREQ(LOOP1) - FREF)
     *               - TWOPI * SB * (GFREQ(LOOP1) - FREF))
                  END IF
               END IF
 90         CONTINUE
         IF (DSUM .GT. DMAX) THEN
            DMAX = DSUM
            IMAX = LOOP
            DLYMAX = DVAL
            END IF
 100     CONTINUE
C                                       Best guess from above
      DMAX = DMAX / ISUM * 100.0
      DVAL = DLYMAX / TWOPI
      ILAST = -1
      DO 200 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).GT.0.0) THEN
C                                       Remove slope estimate
C                                       and leave remainder
            P(LOOP) = PHASE(LOOP) - (DVAL+SB) * (GFREQ(LOOP)-FREF)*TWOPI
            P(LOOP) = MOD (P(LOOP), TWOPI)
            IF (P(LOOP) .LT. -PI) P(LOOP) = P(LOOP) + TWOPI
            IF (P(LOOP) .GT.  PI) P(LOOP) = P(LOOP) - TWOPI
C                                       Add or subtract 2 pi to avoid
C                                       wrap problem.
            IF (ILAST.GT.0) THEN
               IF ((P(LOOP)-P(ILAST)).GT. 3.1) P(LOOP) = P(LOOP) - TWOPI
               IF ((P(LOOP)-P(ILAST)).LT.-3.1) P(LOOP) = P(LOOP) + TWOPI
               END IF
            ILAST = LOOP
            END IF
 200     CONTINUE
C                                       Init sums
      COUNT = 0
      SUMX  = 0.0
      SUMXX = 0.0
      SUMY  = 0.0
      SUMXY = 0.0
C                                       Least squares slope
      DO 300 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).GT.0.0) THEN
            COUNT = COUNT + 1
            SUMX  = SUMX + FREQ(LOOP)
            SUMXX = SUMXX + FREQ(LOOP) * FREQ(LOOP)
            SUMY  = SUMY + P(LOOP)
            SUMXY = SUMXY + FREQ(LOOP) * P(LOOP)
            END IF
 300     CONTINUE
C                                       Slope
      MB = 0.0
      IF (COUNT.LE.0) GO TO 999
      DIV = SUMXX - (SUMX * SUMX / COUNT)
      IF (DIV.EQ.0.0) GO TO 999
      MB = (SUMXY - (SUMX * SUMY / COUNT)) / DIV
C                                       MB uncertainty done right
      MBE = 0.0
      DO 310 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).GT.0.0) THEN
            DTEMP1 = (SUMX - COUNT*FREQ(LOOP)) / DIV
            MBE = MBE + DTEMP1*DTEMP1
            END IF
 310     CONTINUE
      MBE = SQRT (MBE)
C                                       Add in initial guess
      IF (MAXMBD .EQ. 1.0E-20) THEN
         MB = 0.0
         DVAL = 0.0
         END IF
      MB = MB + DVAL * TWOPI
C                                       Must be in first ambiguity
      DAMBA = DAMB * TWOPI
      IF (MB .GE. (0.5D0*DAMBA)) MB = MB - DAMBA
      IF (MB .LE. (-0.5D0*DAMBA)) MB = MB + DAMBA
C                                       Correct PHASE array
C                                       and get average
      DTEMP1 = 0.0
      ISUM = 0
      DO 400 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).LE.0.0) GO TO 400
         PTMP = PHASE(LOOP) - (MB+TWOPI*SB) * (FREQ(LOOP)-FREF)
         ISUM = ISUM + 1
         PTMP = MOD (PTMP, TWOPI)
         IF (PTMP .GT. PI) PTMP = PTMP - TWOPI
         IF (PTMP .LT. -PI) PTMP = PTMP + TWOPI
         IF (ISUM .EQ. 1) THEN
            DTEMP2 = PTMP
         ELSE
            IF (PTMP-DTEMP2.GT.PI) PTMP = PTMP - TWOPI
            IF (PTMP-DTEMP2.LT.-PI) PTMP = PTMP + TWOPI
            END IF
         DTEMP1 = DTEMP1 + PTMP
         P(LOOP) =  PTMP
 400     CONTINUE
      ISUM = MAX (ISUM, 1)
      PTMP = DTEMP1/ISUM
C                                       MB delay to seconds
      MB = MB / TWOPI + SB
C                                       Get rms
      DTEMP1 = 0.0
      ISUM = 0
      DO 410 LOOP = 1,NUMIF
         IF (PHASWT(LOOP).LE.0.0) GO TO 410
         ISUM = ISUM + 1
         DTEMP1 = DTEMP1 + (P(LOOP)-PTMP)**2
         DTEMP2 = PTMP - P(LOOP)
C                                       Average phase
         IF (ISUM .EQ. 1) DTEMP3 = -DTEMP2 - PHASE(LOOP)
         IF (PRTLEV.GE.2) RPHASE(LOOP) = DTEMP2 * RAD2DG
 410     CONTINUE
      ISUM = MAX (1, ISUM)
      DTEMP1 = SQRT (DTEMP1/ISUM)
C                                       delay error
      MBE = MBE * DTEMP1 / TWOPI
C                                       RMS phase resid
      RMSFAZ = DTEMP1
      DTEMP1 = DTEMP1 * RAD2DG
      PTMP = MOD (DTEMP3, TWOPI)
      IF (PTMP .GT. PI) PTMP = PTMP - TWOPI
      IF (PTMP .LT. -PI) PTMP = PTMP + TWOPI
      AVGFAZ = PTMP
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION IFINC (IFFREQ, NIF)
C-----------------------------------------------------------------------
C   The grid spacing for IF frequencies.
C
C   Inputs:
C      IFFREQ   D(*)     IF reference frequencies
C      NIF      I        Number of IFs
C-----------------------------------------------------------------------
      DOUBLE PRECISION IFFREQ(*)
      INTEGER   NIF
C
C   TOL      D        Tolerance used in testing for termination (Hz)
C
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 10.0)
      INTEGER   IMAX, I
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION D(MAXIF), DIFF, MAX, MIN
C
      INTEGER   IDAMAX
      DOUBLE PRECISION DMIN
C-----------------------------------------------------------------------
C                                       Compute the frequency
C                                       differences and use a
C                                       generalized form of Euclid's
C                                       algorithm for finding their
C                                       gcd.
      DO 10 I = 1, NIF - 1
         D(I) = DABS(IFFREQ(I+1) - IFFREQ(I))
   10    CONTINUE
C
   20 CONTINUE
C                                        Find maximum and minimum
C                                        differences
         IMAX = IDAMAX (D, NIF - 1)
         MAX = D(IMAX)
         MIN = DMIN (D, NIF - 1)
C                                        Find range of differences:
         DIFF = MAX - MIN
C                                        Test for termination:
         IF (DIFF.LE.TOL) GO TO 40
C                                        Reduce maximum by minimum
         D(IMAX) = DIFF
         GO TO 20
   40 CONTINUE
C                                        The GCD is what we want:
      IFINC = D(1)
      END
      INTEGER FUNCTION IDAMAX (S, N)
C-----------------------------------------------------------------------
C The index of the first minimum in a single-precision vector
C S of length N.  Compatible with the BLAS1 routine.
C
C Inputs:
C    S     D(*)       The vector
C    N     I          The length of the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION S(*)
      INTEGER   N
C
      INTEGER   I
      DOUBLE PRECISION MAX
C-----------------------------------------------------------------------
      IDAMAX = 1
      MAX = S(1)
      DO 10 I = 2, N
         IF (S(I).GT.MAX) THEN
            MAX = S(I)
            IDAMAX = I
            END IF
   10    CONTINUE
C
      END
      DOUBLE PRECISION FUNCTION DMIN (D, N)
C-----------------------------------------------------------------------
C Return the minimum value in a double precision vector of length N
C
C Inputs:
C    D       D(*)       The vector
C    N       I          The number of elements in the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION D(*)
      INTEGER   N
C
      INTEGER   I
C-----------------------------------------------------------------------
      DMIN = D(1)
      DO 10 I = 2, N
         IF (D(I).LT.DMIN) DMIN = D(I)
   10    CONTINUE
      END
      INTEGER FUNCTION GETREF (NIF, IPOL, REFA, CREAL, WEIGHT)
C-----------------------------------------------------------------------
C   Function to find the most common reference antenna.
C   Inputs:
C      NIF    I      Number of IFs.
C      IPOL   I      Polarization number (1 or 2)
C      REFA   I(2,*) Reference antenna numbers (1 per poln, IF)
C      CREAL  r(2,*) Real part of complex gain, used to check for
C                    blanking (1 per poln, IF)
C      WEIGHT R(2,*) Weight to check for blanking (1 per poln, IF)
C   Output:
C      GETREF I      Most common valid reference antenna.
C-----------------------------------------------------------------------
      INTEGER   NIF, IPOL, REFA(2,*)
      REAL      CREAL(2,*), WEIGHT(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER  ANTCNT(MAXANT), LOOP, IREF, MCNT
C-----------------------------------------------------------------------
C                                       Find most common reference
C                                       antenna.
      CALL FILL (MAXANT, 0, ANTCNT)
      DO 10 LOOP = 1,NIF
         IREF = REFA(IPOL,LOOP)
         IF ((CREAL(IPOL,LOOP).NE.FBLANK) .AND.
     *      (WEIGHT(IPOL,LOOP).GT.0.0).AND. (IREF.GT.0) .AND.
     *      (IREF.LE.MAXANT)) ANTCNT(IREF) = ANTCNT(IREF) + 1
 10      CONTINUE
      IREF = 0
      MCNT = 0
C                                       Which one used most
      DO 20 LOOP = 1,MAXANT
         IF (ANTCNT(LOOP) .GT. MCNT) THEN
            MCNT = ANTCNT(LOOP)
            IREF = LOOP
            END IF
 20      CONTINUE
      GETREF = IREF
C
 999  RETURN
      END
      SUBROUTINE ANTIFO (INTAB, SUBARR, ANAME, REFREQ, IERR)
C-----------------------------------------------------------------------
C   Returns the names of the antennas in a given subarray with
C   associated table INTAB.
C   Inputs:
C      INTAB   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      SUBARR  I    Subarray number 0=> 1
C   Output:
C      ANAME   C(*)*8 Antenna names
C      REFREQ  D    Reference frequency (Hz)
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), ANAME(*)*8
      DOUBLE PRECISION REFREQ
      INTEGER SUBARR, IERR
C
      CHARACTER ANTAB*36, COLLAB(2)*24, CDUMMY*1
      INTEGER   NROW, ANTNO, I, COLS(2), TYPE, DIM(3), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA COLLAB /'NOSTA', 'ANNAME'/
C-----------------------------------------------------------------------
      IERR = 0
C                                        Make AN table object
      ANTAB = 'AN table'
      CALL OCOPY (INTAB, ANTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (ANTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'AN', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL OPUT (ANTAB, 'VER', OOAINT, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL OOPEN (ANTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get reference frequency
      CALL OGET (ANTAB, 'KEY.FREQ', TYPE, DIM, REFREQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Find column numbers
      CALL TABCOL (ANTAB, 2, COLLAB, COLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
C                                        Make sure all columns found
      DO 10 I = 1,2
         IF (COLS(I).LE.0) THEN
            MSGTXT = 'SN TABLE MISSING COLUMN ' // COLLAB(I)
            CALL MSGWRT (9)
            IERR = 7
            END IF
 10      CONTINUE
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (ANTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over table
      DO 100 I = 1,NROW
C                                       Read data for this row
         CALL TABDGT (ANTAB, I, COLS(1), TYPE, DIM, ANTNO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         ANAME(ANTNO) = ' '
         CALL TABDGT (ANTAB, I, COLS(2), TYPE, DIM, DUMMY, ANAME(ANTNO),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Close AN table
      CALL OCLOSE (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete AN object
      CALL DESTRY (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SUINFO (INTAB, SUBARR, NSOUR, SNAME, SSF, IERR)
C-----------------------------------------------------------------------
C   Returns the names of the sources in a given subarray with
C   associated table INTAB.
C   Inputs:
C      INTAB   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      SUBARR  I    Subarray number 0=> 1
C   Output:
C      NSOUR   I    The number of sources
C      SNAME   C(*)*8 Source names
C      SSF     L    Single source file?
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), SNAME(*)*8
      INTEGER SUBARR, IERR, NSOUR, SNO
C
      CHARACTER SUTAB*36, CDUMMY*1
      INTEGER   NROW, TYPE, DIM(3), DUMMY, I
      LOGICAL   SSF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                        Make SU table object
      SUTAB = 'SU table'
      CALL OCOPY (INTAB, SUTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table type
      SSF = .FALSE.
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (SUTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SU', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL OPUT (SUTAB, 'VER', OOAINT, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL OOPEN (SUTAB, 'READ', IERR)
      IF (IERR.NE.0) THEN
C                                       Single source file
         SSF = .TRUE.
         NSOUR = 1
         SNAME(1) = '        '
         IERR = 0
         WRITE (MSGTXT,1000)
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Get number of entries
      CALL OGET (SUTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over table
      DO 100 I = 1,NROW
C                                       Read data for this row
         CALL TABDGT (SUTAB, I, 1, TYPE, DIM, SNO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         NSOUR = MAX (NSOUR, SNO)
         SNAME(SNO) = ' '
         CALL TABDGT (SUTAB, I, 2, TYPE, DIM, DUMMY, SNAME(SNO),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Close SU table
         CALL OCLOSE (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Delete SU object
         CALL DESTRY (SUTAB, IERR)
         IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C---------------------------------------------------------------
 1000 FORMAT ('ASSUMING SINGLE SOURCE FILE')
      END
