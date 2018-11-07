LOCAL INCLUDE 'BPSMO.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      XSEQ, XDISK, XVER, XBCHAN, XECHAN, XDOWT, APARM(10)
      HOLLERITH XINNAM(3), XINCLS(2)
      CHARACTER INNAM*12, INCLS*6
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XBCHAN, XECHAN,
     *   XDOWT, APARM
      COMMON /CHPARM/ INNAM, INCLS
      INTEGER   MAXFQI, MAXSUB
      PARAMETER (MAXFQI=1000)
      PARAMETER (MAXSUB=100)
      INTEGER   INSEQ, INDISK, BPVER, CNO, IUSER, OUTVER, BPBUFO(512),
     *   OBPRNO, ISFQID(MAXFQI), ISANT(MAXANT+1), MAXREC, STYPE,
     *   BPKOLO(MAXBPC), BPNUMO(MAXBPC), ISUBAR(MAXSUB), BCHAN, ECHAN,
     *   DONORM
      DOUBLE PRECISION TIME, TIMBEG, TIMEND
      REAL      WEIGHT(2*MAXIF), BNDPAS(2,MAXCIF)
      LOGICAL   REPAIR
      COMMON /BPSMOC/ TIME, TIMBEG, TIMEND, BPBUFO, INSEQ, INDISK,
     *   BPVER, CNO, IUSER, OUTVER, OBPRNO, ISFQID, ISANT, ISUBAR,
     *   WEIGHT, BNDPAS, MAXREC, BPKOLO, BPNUMO, STYPE, BCHAN, ECHAN,
     *   DONORM, REPAIR
      INCLUDE 'INCS:DBPC.INC'
LOCAL END
      PROGRAM BPSMO
C-----------------------------------------------------------------------
C! Computes bandpass table at regular intervals from input one
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1998, 2005, 2009, 2011-2012
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
C   Smooths/interpolates bandpass table to regular intervals
C   Inputs from user
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      BPVER......Specifies the version of the BP table to be read as
C                 input.   0 -> highest.
C                 The output version is always highest + 1.
C      APARM......Specifies the type of time smoothing to be applied to
C                 the bandpass table.
C              APARM(1) = type of smoothing to apply:
C                 0 => Two-point (linear between nearest neighbors)
C                 1 => Hanning (linear)
C                 2 => Gaussian
C                 3 => Boxcar
C                 4 => Sinc (i.e. sin(x)/x)
C              APARM(2) = the "diameter" of the function in hours, i.e.
C                 width between first nulls of Hanning triangle and sinc
C                 function, FWHM of Gaussian, width of Boxcar
C                 0 => 1 hour.
C              APARM(3) = the diameter over which the smoothing function
C                 has value in hours.  Defaults: 1, 3, 1, 4 times
C                 APARM(2) used when input APARM(3) < net APARM(2).
C              APARM(4) = increment between output bandpass solutions in
C                 hours.  0 -> 1 hour.
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'BPSMO.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL BPSMIN (IRET)
C                                       do smoothing
      IF (IRET.EQ.0) CALL BPSMOT (IRET)
C                                       do history
      IF (IRET.EQ.0) CALL BPSMHI (IRET)
C                                       close down
      CALL DIE (IRET, BPBUFF)
C
 999  STOP
      END
      SUBROUTINE BPSMIN (IERR)
C-----------------------------------------------------------------------
C   BPSMIN performs initialization for AIPS task BPSMO.  It gets the
C   adverbs, opens the catalog file for 'WRIT', sorts and opens the BP
C   input file, creates the BP output file, and determines which FQs,
C   times, and antenna counts apply to the table.
C   Output:
C      IERR    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPSMO.INC'
      CHARACTER INTYP*2, STAT*4, PRGN*6, KEYS(2)*24, LTYPBP*8
      INTEGER   NPARM, IROUND, BPLUN, JERR, I, J, KEY(2,2), NKEY, ANTNO,
     *   SUBA, SOURID, BPREF(2), FREQID, OANTNO, BPLUNO, KOLS(2),
     *   MXFQID(MAXFQI), KEYSUB(2,2)
      REAL      FKEY(2,2), INTERV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BPLUN, BPLUNO, INTYP /27, 28, 'UV'/
      DATA PRGN /'BPSMO '/
      DATA NKEY /2/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'ANTENNA ', 'TIME '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 21
      IERR = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, BPBUFF, JERR)
      IF (JERR.NE.0) THEN
         RQUICK = .TRUE.
         IERR = 8
         IF (JERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) JERR, 'GET INPUT ADVERBS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IERR, BPBUFF, JERR)
      IF (IERR.NE.0) GO TO 999
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      BPVER = IROUND (XVER)
      IUSER = NLUSER
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, BPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file write
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'WRIT', BPBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ CATALOG HEADER'
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 1
C                                       Open BP file
      CALL BPREFM (INDISK, CNO, BPVER, CATBLK, BPLUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL BPINI ('READ', BPBUFF, INDISK, CNO, BPVER, CATBLK, BPLUN,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP,
     *   NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN INPUT BP TABLE'
         GO TO 990
         END IF
      WPOLY = LTYPBP.NE.' '
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS, 24, .TRUE., BPBUFF, KOLS, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FIND BP COLUMNS'
         GO TO 990
         END IF
C                                       Sort to ant-time order
      IF ((BPBUFF(43).NE.KOLS(1)) .OR. (BPBUFF(44).NE.KOLS(2))) THEN
C                                       Close
         CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSE BP TABLE'
            GO TO 990
            END IF
C                                       sort
         KEY(1,1) = KOLS(1)
         KEY(2,1) = KOLS(1)
         KEY(1,2) = KOLS(2)
         KEY(2,2) = KOLS(2)
         CALL TABSRT (INDISK, CNO, 'BP', BPVER, BPVER, KEY, KEYSUB,
     *      FKEY, BPBUFF, CATBLK, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'SORT BP TABLE'
            GO TO 990
            END IF
C                                       Re-open BP table for read
         CALL BPINI ('READ', BPBUFF, INDISK, CNO, BPVER, CATBLK, BPLUN,
     *      IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP,
     *      BCHNBP, NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN INPUT BP TABLE'
            GO TO 990
            END IF
         END IF
C                                       number of records
      NBPINR = BPBUFF(5)
      CALL FILL (MAXANT+1, 0, ISANT)
      CALL FILL (MAXSUB, 0, ISUBAR)
      CALL FILL (MAXFQI, 0, ISFQID)
      CALL FILL (MAXFQI, 0, MXFQID)
      MAXREC = 0
      TIMBEG = 1.E10
      TIMEND = -1.E10
      OANTNO = 0
      DO 50 I = 1,NBPINR
         CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *      CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
            GO TO 990
            END IF
         SUBA = MAX (1, SUBA)
         ISUBAR(SUBA) = ISUBAR(SUBA) + 1
         TIMBEG = MIN (TIMBEG, TIME)
         TIMEND = MAX (TIMEND, TIME)
         IF (ANTNO.NE.OANTNO) THEN
            DO 20 J = 1,MAXFQI
               MAXREC = MAX (MAXREC, MXFQID(J))
               MXFQID(J) = 0
 20            CONTINUE
            ISANT(ANTNO) = I
            OANTNO = ANTNO
            END IF
         IF (FREQID.GT.MAXFQI) THEN
            MSGTXT = 'TOO MANY FQ ENTRIES: QUITTING'
            IERR = 8
            GO TO 990
            END IF
         FREQID = MAX (1, FREQID)
         ISFQID(FREQID) = 1
         MXFQID(FREQID) = MXFQID(FREQID) + 1
 50      CONTINUE
      ISANT(ANTNO+1) = NBPINR+1
      DO 60 J = 1,MAXFQI
         MAXREC = MAX (MAXREC, MXFQID(J))
 60      CONTINUE
C                                       create new BP table
      OUTVER = 0
      CALL BPINI ('WRIT', BPBUFO, INDISK, CNO, OUTVER, CATBLK, BPLUNO,
     *   OBPRNO, BPKOLO, BPNUMO, NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP,
     *   NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATE BP TABLE'
         GO TO 990
         END IF
C                                       set up APARM
      STYPE = IROUND (APARM(1))
      IF ((STYPE.LT.0) .OR. (STYPE.GT.4)) STYPE = 0.
      IF (APARM(2).LE.0.001) APARM(2) = 1.0
      IF (APARM(3).LT.APARM(2)) THEN
         APARM(3) = APARM(2)
         IF (STYPE.EQ.2) APARM(3) = 3.0 * APARM(3)
         IF (STYPE.EQ.4) APARM(3) = 4.0 * APARM(3)
         END IF
      REPAIR = APARM(4).LT.-0.001
      IF (APARM(4).LT.0.001) APARM(4) = 1.0
      APARM(2) = APARM(2) / 24.0
      APARM(3) = APARM(3) / 24.0
      APARM(4) = APARM(4) / 24.0
      IF ((WPOLY) .OR. (APARM(5).GT.0.001)) STYPE = STYPE + 100
C                                       set up normalization
      IF ((XBCHAN.LT.-0.0001) .OR. (WPOLY)) THEN
         BCHAN = -1
         ECHAN = -1
         DONORM = -1
      ELSE
         DONORM = 1
         IF (APARM(5).GT.0.001) DONORM = 2
         BCHAN = IROUND (XBCHAN)
         ECHAN = IROUND (XECHAN)
         IF ((BCHAN.EQ.0) .OR. (BCHAN.GT.NCHNBP)) THEN
            BCHAN = NCHNBP / 8.0 + 0.76
            BCHAN = MAX (1, BCHAN)
            ECHAN = NCHNBP + 1 - BCHAN
         ELSE
            ECHAN = MIN (ECHAN, NCHNBP)
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSMIN: ERROR',I5,' ON ',A)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
      END
      SUBROUTINE BPSMOT (IERR)
C-----------------------------------------------------------------------
C   BPSMOT uses the open BP tables, reading one and writing a smoothed
C   output to the other.
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPSMO.INC'
      INTEGER   I, J, NWT, NBP, J1, J2, IA, ISU, IFQ, NREC, ITI, NT,
     *   SOURID, SUBA, ANTNO, FREQID, SOURD, BREF(2), BPREF(2), NWD,
     *   JERR, NOWAIT, IROUND
      LONGINT   LBP, LWT
      REAL      INTERV, TIMES(4000), INTRV, CHNB
      DOUBLE PRECISION BPFRQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NWT = NPOLBP * NIFBP
      NBP = 2 * NPOLBP * NIFBP * NCHNBP
      NOWAIT = IROUND (XDOWT)
      IF (NOWAIT.EQ.0) NOWAIT = -1
      NOWAIT = MAX (-2, MIN (2, NOWAIT))
C                                       allocate summing memories
      NWD = NBP * MAXREC
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'BPSMOT', NWD, BPBUF, PBPBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GET MEMORY FOR BP DATA'
         GO TO 990
         END IF
      NWD = NWT * MAXREC
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'BPSMOT', NWD, VLBUF, PVLBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GET MEMORY FOR WT DATA'
         GO TO 990
         END IF
C                                       time parameters
      J1 = TIMBEG / APARM(4) + 0.0001
      J2 = (TIMEND - 0.0001) / APARM(4) + 1.0
      IF (J2.LT.J1) THEN
         MSGTXT = 'NO TIMES FOUND'
         IERR = 8
         GO TO 990
         END IF
      TIMBEG = J1 * APARM(4)
      TIMEND = J2 * APARM(4)
      NT = J2 - J1 + 1
C                                       loop over FQ and antenna
      DO 200 IFQ = 1,MAXFQI
         DO 150 ISU = 1,MAXSUB
            DO 100 IA = 1,NANTBP
               NREC = 0
C                                       loop over data records
               IF ((ISFQID(IFQ).GT.0) .AND. (ISANT(IA).GT.0) .AND.
     *            (ISUBAR(ISU).GT.0)) THEN
                  J1 = ISANT(IA)
                  J2 = ISANT(IA+1) - 1
                  LWT = 1 + PVLBUF
                  LBP = 1 + PBPBUF
                  DO 20 J = J1,J2
                     IBPRNO = J
                     CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *                  NIFBP, NCHNBP, NPOLBP, TIME, INTERV, SOURID,
     *                  SUBA, ANTNO, CHNBND, BPFREQ, FREQID, BPREF,
     *                  WEIGHT, BNDPAS, IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
                        GO TO 990
                        END IF
                     IF (((FREQID.EQ.IFQ) .OR. (FREQID.LE.0)) .AND.
     *                  ((SUBA.EQ.ISU) .OR. (SUBA.LE.0)) .AND.
     *                  (ANTNO.EQ.IA)) THEN
                        NREC = NREC + 1
                        IF (NREC.EQ.1) THEN
                           SOURD = SOURID
                           CHNB = CHNBND
                           BREF(1) = BPREF(1)
                           BREF(2) = BPREF(2)
                           INTRV = INTERV
                           DO 10 I = 1,NIFBP
                              BPFRQ(I) = BPFREQ(I)
 10                           CONTINUE
                           END IF
                        TIMES(NREC) = TIME
                        CALL RCOPY (NWT, WEIGHT, VLBUF(LWT))
                        CALL RCOPY (NBP, BNDPAS, BPBUF(LBP))
                        LBP = LBP + NBP
                        LWT = LWT + NWT
                        END IF
 20                  CONTINUE
                  END IF
               IF (NREC.GT.0) THEN
                  LWT = 1 + PVLBUF
                  LBP = 1 + PBPBUF
                  TIME = TIMBEG - APARM(4)
                  IF (REPAIR) NT = NREC
                  DO 25 ITI = 1,NT
                     IF (REPAIR) THEN
                        TIME = TIMES(ITI)
                     ELSE
                        TIME = TIME + APARM(4)
                        END IF
                     CALL SMTHIT (TIME, STYPE, APARM(2), DONORM, BCHAN,
     *                  ECHAN, NIFBP, NCHNBP, NPOLBP, NREC, TIMES,
     *                  VLBUF(LWT), BPBUF(LBP), ITI, REPAIR, NOWAIT,
     *                  WEIGHT, BNDPAS)
                     CALL TABBP ('WRIT', BPBUFO, OBPRNO, BPKOLO, BPNUMO,
     *                  NIFBP, NCHNBP, NPOLBP, TIME, INTRV, SOURD, ISU,
     *                  IA, CHNB, BPFRQ, IFQ, BREF, WEIGHT, BNDPAS,
     *                  IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (MSGTXT,1000) IERR, 'READ BP TABLE'
                        GO TO 990
                        END IF
 25                  CONTINUE
                  END IF
 100           CONTINUE
 150        CONTINUE
 200     CONTINUE
C                                       close
      CALL TABBP ('CLOS', BPBUFO, OBPRNO, BPKOLO, BPNUMO, NIFBP, NCHNBP,
     *   NPOLBP, TIME, INTRV, SOURD, ISU, IA, CHNB, BPFRQ, IFQ, BREF,
     *   WEIGHT, BNDPAS, JERR)
      CALL TABBP ('CLOS', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP, NCHNBP,
     *   NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO, CHNBND, BPFREQ,
     *   FREQID, BPREF, WEIGHT, BNDPAS, JERR)
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  CALL ZMEMRY ('QUIT', 'BPSMOT', NWD, BPBUF, PBPBUF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSMOT: ERROR',I5,' ON ',A)
      END
      SUBROUTINE SMTHIT (TIME, STYPE, WIDTHS, DONORM, BCHAN, ECHAN,
     *   NIFBP, NCHNBP, NPOLBP, NREC, TIMES, WTBUF, BPBUF, ITI, REPAIR,
     *   DOWAIT, WEIGHT, BNDPAS)
C-----------------------------------------------------------------------
C   SMTHIT does all the work.
C   Inputs:
C      TIME     D      Time for output sample
C      STYPE    I      Smoothing type
C      WIDTHS   R(2)   Diameter and support of function in days
C      DONORM   I      Normalization control: -1 no, 1 scalar, 2 vector
C      BCHAN    I      Begin channel of normalization
C      ECHAN    I      End channel of normalization
C      NIFBP    I      Number of IFs
C      NCHNBP   I      Number of channels
C      NPOLBP   I      Number of polarizations
C      NREC     I      Number of times
C      TIMES    R(*)   Times   : R(NREC)
C      WTBUF    R(*)   Weights : R(NIFBP,NPOLBP,NREC)
C      BPBUF    C(*)   Bandpass: C(NCHNBP,NIFBP,NPOLBP,NREC)
C      ITI      I      Output matches the ITI record in WTBUF, BPBUF
C                      when REPAIR
C      REPAIR   L      T => change only if flagged
C      DOWAIT   I      < 0 => Ignore data weights in interpolation wts
C                      -1,1 => interpolate weights,
C                      -2,2 +> weight combination w weights
C   Output:
C      WEIGHT   R(*)   Weight  : R(NIFBP,NPOLBP)
C      BNDPAS   C(*)   Bandpass: C(NCHNBP,NIFBP,NPOLBP)
C   Changed to interpolate the weights rather than to do the full
C   noise combination formula which gives an intermediate (averaged)
C   point more weight than its parents.  The other form is left in
C   commented code lines.
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   STYPE, DONORM, BCHAN, ECHAN, NIFBP, NCHNBP, NPOLBP,
     *   NREC, ITI, DOWAIT
      REAL      WIDTHS(2), TIMES(NREC), WTBUF(NIFBP,NPOLBP,NREC),
     *   BPBUF(2,NCHNBP,NIFBP,NPOLBP,NREC), WEIGHT(NIFBP,NPOLBP),
     *   BNDPAS(2,NCHNBP,NIFBP,NPOLBP)
      LOGICAL   REPAIR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LI, LC, LP, LR, LR1, LR2, NW, NB, LSTYPE
      REAL      SR, DR, W, F, WFSUM, FFSUM, F2, W2, BR1, BR2, BI1, BI2,
     *   AX, A1, A2, WTSUM(MAXCHA), AMPSUM(MAXCHA)
      LOGICAL   SCALAR, NOWAIT, WTWAIT
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NW = NIFBP * NPOLBP
      NB = 2 * NCHNBP * NW
      NOWAIT = DOWAIT.LE.0
      WTWAIT = ABS(DOWAIT).EQ.2
C                                       copy to output
      IF (REPAIR) THEN
         CALL RCOPY (NW, WTBUF(1,1,ITI), WEIGHT)
         CALL RCOPY (NB, BPBUF(1,1,1,1,ITI), BNDPAS)
C                                       are any flagged?
         DO 15 LP = 1,NPOLBP
            DO 10 LI = 1,NIFBP
               IF (WTBUF(LI,LP,ITI).LE.0.0) GO TO 20
 10            CONTINUE
 15         CONTINUE
         GO TO 999
         END IF
C                                       something to do
 20   SR = WIDTHS(2) / 2.0
      DR = WIDTHS(1) / 2.0
      LSTYPE = MOD (STYPE, 100)
      SCALAR = STYPE.LT.100
C                                       find times
      IF (LSTYPE.NE.0) THEN
         LR1 = NREC
         LR2 = 1
         DO 25 LR = 1,NREC
            IF (TIME-SR.LT.TIMES(LR)) LR1 = MIN (LR1, LR)
            IF (TIME+SR.GT.TIMES(LR)) LR2 = LR
 25         CONTINUE
         END IF
      NB = 2 * NCHNBP
C                                       loop over pol and IF
      DO 200 LP = 1,NPOLBP
         DO 195 LI = 1,NIFBP
            IF ((WTBUF(LI,LP,ITI).LE.0.0) .OR. (.NOT.REPAIR)) THEN
C                                       nearest neighbor
               IF (LSTYPE.EQ.0) THEN
C                                       find the nearest good ones
                  LR1 = 0
                  LR2 = NREC + 1
                  DO 100 LR = 1,NREC
                     IF (WTBUF(LI,LP,LR).GT.0.0) THEN
                        IF (TIME.GE.TIMES(LR)) LR1 = LR
                        IF (TIME.LE.TIMES(LR)) LR2 = MIN (LR, LR2)
                        END IF
 100                 CONTINUE
                  IF (LR1.LT.1) THEN
                     IF (LR2.GT.NREC) THEN
                        WEIGHT(LI,LP) = 0.0
                        CALL RFILL (NB, FBLANK, BNDPAS(1,1,LI,LP))
                        GO TO 195
                        END IF
                     LR1 = LR2
                  ELSE IF (LR2.GT.NREC) THEN
                     LR2 = LR1
                     END IF
C                                       only one
                  IF (LR1.EQ.LR2) THEN
                     WEIGHT(LI,LP) = WTBUF(LI,LP,LR1)
                     CALL RCOPY (NB, BPBUF(1,1,LI,LP,LR1),
     *                  BNDPAS(1,1,LI,LP))
C                                       linear interpolation
                  ELSE
                     SR = TIMES(LR2) - TIMES(LR1)
                     W = WTBUF(LI,LP,LR1)
                     W2 = WTBUF(LI,LP,LR2)
                     F = 1.0
                     IF (SR.GT.0.0) THEN
                        F = (TIMES(LR2) - TIME) / SR
                        F = MAX (0.0, MIN (1.0, F))
                        END IF
                     F2 = 1.0 - F
                     IF (NOWAIT) THEN
                        IF (WTWAIT) THEN
                           FFSUM = F * F / W  +  F2 * F2 / W2
                           WEIGHT(LI,LP) = 1.0 / FFSUM
                        ELSE
                           WEIGHT(LI,LP) = W * F + W2 * F2
                           END IF
                        W = F
                        W2 = F2
                     ELSE
                        WFSUM = W * F + W2 * F2
                        IF (WTWAIT) THEN
                           FFSUM = W * F * F  +  W2 * F2 * F2
                           WEIGHT(LI,LP) = WFSUM * WFSUM / FFSUM
                        ELSE
                           WEIGHT(LI,LP) = (W*W*F + W2*W2*F2) / WFSUM
                           END IF
                        W = W * F / WFSUM
                        W2 = W2 * F2 / WFSUM
                        END IF
                     DO 110 LC = 1,NCHNBP
                        BR1 = BPBUF(1,LC,LI,LP,LR1)
                        BR2 = BPBUF(1,LC,LI,LP,LR2)
                        BI1 = BPBUF(2,LC,LI,LP,LR1)
                        BI2 = BPBUF(2,LC,LI,LP,LR2)
                        IF ((BR1.NE.FBLANK) .AND. (BR2.NE.FBLANK) .AND.
     *                     (BI1.NE.FBLANK) .AND. (BI2.NE.FBLANK)) THEN
                           BNDPAS(1,LC,LI,LP) = W * BR1 + W2 * BR2
                           BNDPAS(2,LC,LI,LP) = W * BI1 + W2 * BI2
                           IF (SCALAR) THEN
                              AX = SQRT (BNDPAS(1,LC,LI,LP)**2 +
     *                           BNDPAS(2,LC,LI,LP)**2)
                              A1 = SQRT (BR1*BR1 + BI1*BI1)
                              A2 = SQRT (BR2*BR2 + BI2*BI2)
                              AX = (W * A1 + W2 * A2) / AX
                              BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP)*AX
                              BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP)*AX
                              END IF
                        ELSE IF ((BR1.NE.FBLANK) .AND. (BI1.NE.FBLANK))
     *                     THEN
                           BNDPAS(1,LC,LI,LP) = BR1
                           BNDPAS(2,LC,LI,LP) = BI1
                        ELSE
                           BNDPAS(1,LC,LI,LP) = BR2
                           BNDPAS(2,LC,LI,LP) = BI2
                           END IF
 110                    CONTINUE
                     END IF
C                                       function smoothing
               ELSE IF ((LSTYPE.GE.1) .AND. (LSTYPE.LE.4)) THEN
                  CALL RFILL (NB, 0.0, BNDPAS(1,1,LI,LP))
                  WEIGHT(LI,LP) = 0.0
                  WFSUM = 0.0
                  FFSUM = 0.0
                  CALL RFILL (NCHNBP, 0.0, WTSUM)
                  IF (SCALAR) CALL RFILL (NCHNBP, 0.0, AMPSUM)
                  DO 120 LR = LR1,LR2
                     SR = ABS (TIME - TIMES(LR))
                     CALL SFUNC (LSTYPE, DR, SR, F)
                     W2 = WTBUF(LI,LP,LR)
                     IF (W2.GT.0.0) THEN
                        IF (NOWAIT) THEN
                           W = F
                        ELSE
                           W = W2 * F
                           END IF
                        WFSUM = WFSUM + W
                        IF (WTWAIT) THEN
                           FFSUM = FFSUM + W * W / W2
                        ELSE
                           FFSUM = FFSUM + W2 * W
                           END IF
                        DO 115 LC = 1,NCHNBP
                           BR1 = BPBUF(1,LC,LI,LP,LR)
                           BI1 = BPBUF(2,LC,LI,LP,LR)
                           IF ((BR1.NE.FBLANK) .AND. (BI1.NE.FBLANK))
     *                        THEN
                              BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP) +
     *                           W*BR1
                              BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP) +
     *                           W*BI1
                              WTSUM(LC) = WTSUM(LC) + W
                              IF (SCALAR) AMPSUM(LC) = AMPSUM(LC) +
     *                           W * SQRT (BR1*BR1 + BI1*BI1)
                              END IF
 115                       CONTINUE
                        END IF
 120                 CONTINUE
C                                       scale
                  IF (WTWAIT) THEN
                     WEIGHT(LI,LP) = WFSUM * WFSUM / FFSUM
                  ELSE
                     WEIGHT(LI,LP) = FFSUM /  WFSUM
                     END IF
                  DO 130 LC = 1,NCHNBP
                     IF (WTSUM(LC).LE.0.0) THEN
                        BNDPAS(1,LC,LI,LP) = FBLANK
                        BNDPAS(2,LC,LI,LP) = FBLANK
                     ELSE
                        BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP) /
     *                     WTSUM(LC)
                        BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP) /
     *                     WTSUM(LC)
                        IF (SCALAR) THEN
                           A1 = SQRT (BNDPAS(1,LC,LI,LP)**2 +
     *                        BNDPAS(2,LC,LI,LP)**2)
                           AX = AMPSUM(LC) / WTSUM(LC)
                           IF (A1.GT.0.0) AX = AX / A1
                           BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP) * AX
                           BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP) * AX
                           END IF
                        END IF
 130                 CONTINUE
                  END IF
C                                       normalize: scalar
               IF (DONORM.EQ.1) THEN
                  F = 0.0
                  W = 0.0
                  DO 140 LC = BCHAN,ECHAN
                     IF (BNDPAS(1,LC,LI,LP).NE.FBLANK) THEN
                        F = F + SQRT (BNDPAS(1,LC,LI,LP)**2 +
     *                     BNDPAS(2,LC,LI,LP)**2)
                        W = W + 1.0
                        END IF
 140                 CONTINUE
                  IF ((W.GT.0.0) .AND. (F.GT.0.0)) THEN
                     F = F / W
                     DO 150 LC = 1,NCHNBP
                        IF (BNDPAS(1,LC,LI,LP).NE.FBLANK)
     *                     BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP) / F
                        IF (BNDPAS(2,LC,LI,LP).NE.FBLANK)
     *                     BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP) / F
 150                    CONTINUE
                     END IF
C                                       normalize: vector
               ELSE IF (DONORM.EQ.2) THEN
                  A1 = 0.0
                  A2 = 0.0
                  W = 0.0
                  DO 160 LC = BCHAN,ECHAN
                     IF (BNDPAS(1,LC,LI,LP).NE.FBLANK) THEN
                        A1 = A1 + BNDPAS(1,LC,LI,LP)
                        A2 = A2 + BNDPAS(1,LC,LI,LP)
                        W = W + 1.0
                        END IF
 160                 CONTINUE
                  F = SQRT (A1*A1 + A2*A2)
                  IF ((W.GT.0.0) .AND. (F.GT.0.0)) THEN
                     F = F / W
                     DO 170 LC = 1,NCHNBP
                        IF (BNDPAS(1,LC,LI,LP).NE.FBLANK)
     *                     BNDPAS(1,LC,LI,LP) = BNDPAS(1,LC,LI,LP) / F
                        IF (BNDPAS(2,LC,LI,LP).NE.FBLANK)
     *                     BNDPAS(2,LC,LI,LP) = BNDPAS(2,LC,LI,LP) / F
 170                    CONTINUE
                     END IF
                  END IF
               END IF
 195        CONTINUE
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SFUNC (TYPE, XR, X, F)
C-----------------------------------------------------------------------
C   Evaluates a function of TYPE with function radius XR at X, returning
C   value in F
C   Inputs:
C      TYPE   I   Type: linear (Hanning), Gaussian, Boxcar, Sinc
C      XR     R   Radius of function
C      X      R   Location to evaluate the function
C   Output:
C      F      R   Function value
C-----------------------------------------------------------------------
      INTEGER   TYPE
      REAL      XR, X, F
C
      REAL      S
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      F = 0.0
C                                       Hanning
      IF (TYPE.EQ.1) THEN
         F = 1.0 - X / XR
         F = MAX (0.0, F)
C                                       Gaussian
      ELSE IF (TYPE.EQ.2) THEN
         S = LOG (2.0) / (XR * XR)
         F = EXP (- S * X * X)
C                                       Boxcar
      ELSE IF (TYPE.EQ.3) THEN
         IF (X.LT.XR) F = 1.0
C                                       Sinc
      ELSE IF (TYPE.EQ.4) THEN
         S = TWOPI * X / XR
         IF (S.EQ.0.0) THEN
            F = 1.0
         ELSE
            F = SIN (S) / S
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE BPSMHI (IRET)
C-----------------------------------------------------------------------
C   BPSMHI writes some history records to record what we did
C   Output:
C      IRET   I   Error code - always 0
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'BPSMO.INC'
      INTEGER   IERR, HIBUFF(256), IT(3), ID(3), IHLUN, LSTYPE, NF
      CHARACTER HILINE*72, ATIME*8, ADATE*12, CVECT(2)*6, CFUNC(5)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IHLUN /29/
      DATA CVECT /'SCALAR', 'VECTOR'/
      DATA CFUNC /'2-POINT', 'HANNING', 'GAUSSIAN', 'BOXCAR',
     *   'SINX / X'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialize HI common
      CALL HIINIT (3)
C                                       Open HI file
      CALL HIOPEN (IHLUN, INDISK, CNO, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.5) THEN
            WRITE (MSGTXT,1000)
         ELSE
            WRITE (MSGTXT,1010) IERR
            END IF
         GO TO 990
         END IF
C                                       Header text
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1020) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 90
C                                       versions
      WRITE (HILINE,1025) TSKNAM, BPVER, OUTVER
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 90
C                                       function type
      LSTYPE = MOD (STYPE, 100) + 1
      NF = 1
      IF (STYPE.GE.100) NF = 2
      APARM(4) = APARM(4) * 24.0
      WRITE (HILINE,1030) TSKNAM, CVECT(NF), CFUNC(LSTYPE), APARM(4)
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 90
C                                       width and support
      IF ((LSTYPE.GT.1) .AND. (LSTYPE.LE.5)) THEN
         APARM(2) = APARM(2) * 24.0
         APARM(3) = APARM(3) * 24.0
         WRITE (HILINE,1035) TSKNAM, APARM(2), APARM(3)
         CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
         IF (IERR.NE.0) GO TO 90
         END IF
C                                       normalization
      IF (DONORM.GT.0) THEN
         WRITE (HILINE,1040) TSKNAM, CVECT(DONORM), BCHAN, ECHAN
      ELSE
         WRITE (HILINE,1041) TSKNAM
         END IF
      CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
      IF (IERR.NE.0) GO TO 90
      GO TO 100
C                                       ERROR
 90   WRITE (MSGTXT,1090) IERR
      CALL MSGWRT (6)
C                                       Close file updating it
 100  CALL HICLOS (IHLUN, .TRUE., HIBUFF, IERR)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSMHI: HISTORY FILE DOES NOT EXIST')
 1010 FORMAT ('BPSMHI: ERROR',I5,' OPENING THE HISTORY FILE')
 1020 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1025 FORMAT (A6,'INVER =',I5,'   OUTVER =',I5,3X,
     *   '/ In/out BP file version numbers')
 1030 FORMAT (A6,'/ ',A,1X,A,' smoothing function every',F7.3,' hours')
 1035 FORMAT (A6,'FWIDTH=',F7.3,'  FSUPPORT=',F7.3,3X,
     *   '/ function width/support hours')
 1040 FORMAT (A6,'/ ',A,' normalization BCHAN=',I5,'  ECHAN=',I5)
 1041 FORMAT (A6,'/ no amplitude normalization')
 1090 FORMAT ('BPSMHI: ERROR',I5,' ADDING TO THE HISTORY FILE')
      END
