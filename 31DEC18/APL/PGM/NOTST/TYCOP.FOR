LOCAL INCLUDE 'TYCOP.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMIN(3), XCLAIN(2), XEXTYP, XOPTYP
      REAL      XSEQIN, XDISKI, XIVER, APARM(10), BPARM(10), XANT(50)
      INTEGER   SEQIN, DISKIN, CNO, BUFF1(512), BUFF2(512), IVER, OVER,
     *   NA, NB, IA(10), IB(10), CDIVER, CDOVER, LUN1, LUN2, IP1, IP2
      LOGICAL   SWAP, DOANT(MAXANT)
      CHARACTER  NAMEIN*12, CLAIN*6, EXTYPE*2, OPTYPE*4
      COMMON /INPARM/ XNAMIN, XCLAIN, XSEQIN, XDISKI, XEXTYP, XIVER,
     *   APARM, BPARM, XOPTYP, XANT
      COMMON /TYCOPI/ BUFF1, BUFF2, SEQIN, DISKIN, CNO, IVER, OVER,
     *   CDIVER, CDOVER, LUN1, LUN2, NA, NB, IA, IB, IP1, IP2, SWAP,
     *   DOANT
      COMMON /TYCOPC/ NAMEIN, CLAIN, EXTYPE, OPTYPE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM TYCOP
C-----------------------------------------------------------------------
C! Task to copy calibration in SY/CD tables between IFs
C# EXT-util Utility Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2011, 2013, 2015, 2017
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
C   TYCOP copies AIPS SY and CD extension files, averaging some IFs and
C   replacing other IFs with the average.  Also TY tables.
C   ADVERBS:
C      INNAME      Input image name (name)
C      INCLASS     Input image name (class)
C      INSEQ       Input image name (seq. #)
C      INDISK      Input image disk unit #
C      INVERS      Input table file version no.
C      APARM       IFs to average
C      BPARM       IFs to replace
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'TYCOP.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       init
      CALL TYCOIN (IRET)
C                                       DO
      IF (IRET.EQ.0) THEN
         IF (EXTYPE.EQ.'SY') THEN
            CALL TYCOSY (IRET)
         ELSE
            CALL TYCOTY (IRET)
            END IF
C                                       History
         IF (IRET.EQ.0) CALL TYCOHI
         END IF
C
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TYCOIN (IRET)
C-----------------------------------------------------------------------
C   TYCOIN does start up operations for TYCOP
C   Output:
C      IRET   I   > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER PRGM*6, FTYPE*2, STAT*4
      INTEGER   IERR, I, IROUND, NPARM, J
      LOGICAL   WASNEG
      INCLUDE 'TYCOP.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGM /'TYCOP '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 80
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'GETTING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XEXTYP, EXTYPE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF (OPTYPE.EQ.'R') THEN
         IP1 = 1
         IP2 = 1
         SWAP = .FALSE.
      ELSE IF (OPTYPE.EQ.'L') THEN
         IP1 = 2
         IP2 = 2
         SWAP = .FALSE.
      ELSE IF (OPTYPE.EQ.'R2L') THEN
         IP1 = 1
         IP2 = 1
         SWAP = .TRUE.
      ELSE IF (OPTYPE.EQ.'L2R') THEN
         IP1 = 2
         IP2 = 2
         SWAP = .TRUE.
      ELSE IF (OPTYPE.EQ.'AR2L') THEN
         IP1 = 1
         IP2 = 1
         SWAP = .TRUE.
      ELSE IF (OPTYPE.EQ.'AL2R') THEN
         IP1 = 2
         IP2 = 2
         SWAP = .TRUE.
      ELSE
         IP1 = 1
         IP2 = 2
         SWAP = .FALSE.
         OPTYPE = ' '
         END IF
C                                       Crunch input parameters.
      SEQIN  = IROUND (XSEQIN)
      DISKIN = IROUND (XDISKI)
      IVER   = IROUND (XIVER)
      IF (OPTYPE(:1).EQ.'A') THEN
         NA = 0
         NB = 0
      ELSE
         NA = 10
         NB = 10
         DO 10 I = 1,10
            IA(I) = IROUND (APARM(I))
            IB(I) = IROUND (BPARM(I))
            IF (IA(I).LE.0) NA = MIN (NA, I-1)
            IF (IB(I).LE.0) NB = MIN (NB, I-1)
 10         CONTINUE
         IF ((NA.LE.0) .OR. (NB.LE.0)) THEN
            MSGTXT = 'YOU MUST SPECIFY IFS IN APARM AND BPARM'
            IRET = 10
            GO TO 990
            END IF
         END IF
      CNO = 0
      WASNEG = .FALSE.
      CALL LFILL (MAXANT, .FALSE., DOANT)
      DO 20 I = 1,50
         J = IROUND (XANT(I))
         IF ((J.GT.0) .AND. (J.LE.MAXANT)) THEN
            DOANT(J) = .TRUE.
            CNO = CNO + 1
         ELSE IF ((J.LT.0) .AND. (J.GE.-MAXANT)) THEN
            DOANT(-J) = .TRUE.
            CNO = CNO + 1
            WASNEG = .TRUE.
         ELSE
            GO TO 25
            END IF
 20      CONTINUE
 25   IF (CNO.EQ.0) CALL LFILL (MAXANT, .TRUE., DOANT)
      IF (WASNEG) THEN
         DO 30 I = 1,MAXANT
            DOANT(I) = .NOT.DOANT(I)
 30         CONTINUE
         END IF
C                                       Find input
      CNO = 1
      FTYPE = 'UV'
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
         WRITE (MSGTXT,1000) IERR, 'READING FILE HEADER'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Check number of tables
      CALL FNDEXT ('SY', CATBLK, I)
      CALL FNDEXT ('TY', CATBLK, J)
      IF ((EXTYPE.NE.'SY') .AND. (EXTYPE.NE.'TY')) THEN
         IF ((I.LE.0) .AND. (J.GT.0)) THEN
            EXTYPE = 'TY'
         ELSE IF ((I.GT.0) .AND. (J.LE.0)) THEN
            EXTYPE = 'SY'
         ELSE
            MSGTXT = 'UNABLE TO DETERMINE INEXT'
            GO TO 990
            END IF
         END IF
      IF (EXTYPE.EQ.'TY') I = J
      IF (I.LE.0) THEN
         MSGTXT = 'DATA SET HAS NO ' // EXTYPE // ' TABLES'
         GO TO 990
         END IF
      IF ((IVER.LE.0) .OR. (IVER.GT.I)) IVER = I
      OVER = I + 1
      CALL FNDEXT ('CD', CATBLK, CDIVER)
      CDOVER = CDIVER + 1
C
      LUN1 = 27
      LUN2 = 28
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYCOIN: ERROR',I3,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
      END
      SUBROUTINE TYCOHI
C-----------------------------------------------------------------------
C   TYCOHI ipdates the HI files for this operation
C-----------------------------------------------------------------------
C
      INTEGER   IRET, TIME(3), DATE(3), I, LA(50), IROUND, JA, IP, L
      LOGICAL   OMIT
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INCLUDE 'TYCOP.INC'
C-----------------------------------------------------------------------
C                                       Add history to output
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKIN, CNO, BUFF1, IRET)
      IF (IRET.GT.2) THEN
         MSGTXT = 'COULD NOT OPEN HI FILE TO ADD UPDATES'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (LUN1, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Input file
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN1, BUFF1,
     *   IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Type and version
      WRITE (HILINE,1010) TSKNAM, EXTYPE, IVER, OVER
      CALL HIADD (LUN1, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (MSGTXT,1011) EXTYPE, IVER, OVER
      CALL MSGWRT (4)
      IF (EXTYPE.EQ.'SY') THEN
         WRITE (HILINE,1010) TSKNAM, 'CD', CDIVER, CDOVER
         CALL HIADD (LUN1, HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (MSGTXT,1011) 'CD', CDIVER, CDOVER
         CALL MSGWRT (4)
         END IF
C                                       IFs
      IF (OPTYPE(:1).NE.'A') THEN
         WRITE (HILINE,1020) TSKNAM, IA
         CALL HIADD (LUN1, HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (MSGTXT,1021) (IA(I), I = 1,NA)
         CALL MSGWRT (4)
         WRITE (HILINE,1030) TSKNAM, IB
         CALL HIADD (LUN1, HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (MSGTXT,1031) (IB(I), I = 1,NB)
         CALL MSGWRT (4)
         END IF
C                                       OPTYPE
      WRITE (HILINE,1040) TSKNAM, OPTYPE
      CALL HIADD (LUN1, HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (MSGTXT,1041) OPTYPE
      CALL MSGWRT (4)
C                                       antennas
      JA = 0
      OMIT = .FALSE.
      DO 50 I = 1,50
         L = IROUND (XANT(I))
         IF (L.LT.0) THEN
            OMIT = .TRUE.
            L = -L
         ELSE IF (L.EQ.0) THEN
            GO TO 55
            END IF
         DO 45 IP = 1,JA
            IF (L.EQ.LA(IP)) GO TO 50
 45         CONTINUE
         JA = JA + 1
         LA(JA) = L
 50      CONTINUE
 55   IF (JA.LE.0) THEN
         WRITE (HILINE,1050) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (MSGTXT,1051)
         CALL MSGWRT (4)
      ELSE
         HILINE = TSKNAM // 'ANTENNAS ='
         IF (OMIT) THEN
            HILINE(63:) = '/ omitted'
            MSGTXT = 'Omitted antennas'
         ELSE
            HILINE(63:) = '/ included'
            MSGTXT = 'Included antennas'
            END IF
         IP = 17
         I = 1
 60      IF (I.LE.JA) THEN
            WRITE (HILINE(IP:IP+2),1060) LA(I)
            WRITE (MSGTXT(IP+1:IP+3),1060) LA(I)
            IP = IP + 3
            IF (I.LT.JA) THEN
               HILINE(IP:IP) = ','
               IP = IP + 1
               END IF
            IF ((IP.GE.58) .OR. (I.EQ.JA)) THEN
               CALL HIADD (LUN1, HILINE, BUFF1, IRET)
               IF (IRET.NE.0) GO TO 100
               CALL MSGWRT (4)
               HILINE(:62) = TSKNAM
               MSGTXT(18:) = ' '
               IP = 17
               END IF
            I = I + 1
            GO TO 60
            END IF
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN1, .TRUE., BUFF1, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'Release =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1010 FORMAT (A6,'TABLE=''',A2,''' INVERS=',I5,' OUTVERS=', I5)
 1011 FORMAT ('Copied ',A2,' table version',I5,' to version',I5)
 1020 FORMAT (A6,'APARM=',9(I2,','),I2,'  / IFs averaged')
 1021 FORMAT ('Averaged IFs',10I5)
 1030 FORMAT (A6,'BPARM=',9(I2,','),I2,'  / IFs replaced')
 1031 FORMAT ('Replaced IFs',10I5)
 1040 FORMAT (A6,'OPTYPE=''',A4,'''  / polarizations used')
 1041 FORMAT ('Under OPTYPE = ''',A,'''')
 1050 FORMAT (A6,'ANTENNAS = 0',5X,'/ all antennas included')
 1051 FORMAT ('For all antennas')
 1060 FORMAT (I3)
      END
      SUBROUTINE TYCOSY (IRET)
C-----------------------------------------------------------------------
C   Routine to average SY and CD values over some IFs and place in
C   other IFs
C   Output:
C      IRET     I       Return error code  0 => ok
C                          else quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER RDATE*8
      INTEGER   IRNO, KOLS(12), NUMV(12), NUMANT, NUMPOL, NUMIF, SOURID,
     *   ANTNO, SUBA, FREQID, IROW, NROW, ND, NS, NG, II, IP, I, MD,
     *   CALTYP
      REAL      TIMEI, PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF),
     *   SD, SS, SG, TCAL(4,MAXIF), SOLD
      DOUBLE PRECISION TIME
      INCLUDE 'TYCOP.INC'
C-----------------------------------------------------------------------
C                                       SY table
      CALL SYINI ('READ', BUFF1, DISKIN, CNO, IVER, CATBLK, LUN1, IRNO,
     *   KOLS, NUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT SY TABLE'
         GO TO 990
         END IF
      IF (NUMPOL.EQ.1) THEN
         IF ((OPTYPE.NE.'R') .AND. (OPTYPE.NE.' ')) THEN
            MSGTXT = '1 POLARIZATION ONLY - OPTYPE=''' // OPTYPE //
     *         ' ILLEGAL'
            IRET = 10
            GO TO 990
            END IF
         IP2 = 1
         END IF
      CALL SYINI ('WRIT', BUFF2, DISKIN, CNO, OVER, CATBLK, LUN2, IRNO,
     *   KOLS, NUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT SY TABLE'
         GO TO 990
         END IF
      NROW = BUFF1(5)
      DO 50 IROW = 1,NROW
         IRNO = IROW
         CALL TABSY ('READ', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID, PDIFF,
     *      PSUM, PGAIN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT SY TABLE'
            GO TO 990
            END IF
         IF (DOANT(ANTNO)) THEN
            IF (OPTYPE(:1).NE.'A') THEN
               DO 30 IP = IP1,IP2
                  ND = 0
                  NS = 0
                  NG = 0
                  SD = 0.0
                  SS = 0.0
                  SG = 0.0
                  DO 10 I = 1,NA
                     II = IA(I)
                     IF ((PDIFF(IP,II).NE.FBLANK) .AND.
     *                  (PDIFF(IP,II).GT.0.0))THEN
                        ND = ND + 1
                        SD = SD + PDIFF(IP,II)
                        END IF
                     IF ((PSUM(IP,II).NE.FBLANK) .AND.
     *                  (PSUM(IP,II).GT.0.0)) THEN
                        NS = NS + 1
                        SS = SS + PSUM(IP,II)
                        END IF
                     IF ((PGAIN(IP,II).NE.FBLANK) .AND.
     *                  (PGAIN(IP,II).GT.0.0)) THEN
                        NG = NG + 1
                        SG = SG + PGAIN(IP,II)
                        END IF
 10                  CONTINUE
                  IF (ND.GT.0) THEN
                     SD = SD / ND
                  ELSE
                     SD = FBLANK
                     END IF
                  IF (NS.GT.0) THEN
                     SS = SS / NS
                  ELSE
                     SS = FBLANK
                     END IF
                  IF (NG.GT.0) THEN
                     SG = SG / NG
                  ELSE
                     SG = FBLANK
                     END IF
                  DO 20 I = 1,NB
                     II = IB(I)
                     IF (SWAP) THEN
                        PDIFF(3-IP,II) = SD
                        PSUM(3-IP,II) = SS
                        PGAIN(3-IP,II) = SG
                     ELSE
                        PDIFF(IP,II) = SD
                        PSUM(IP,II) = SS
                        PGAIN(IP,II) = SG
                        END IF
 20                  CONTINUE
 30               CONTINUE
C                                       all IFs transfer
            ELSE
               DO 40 IP = IP1,IP2
                  DO 35 II = 1,NUMIF
                     PDIFF(3-IP,II) = PDIFF(IP,II)
                     PSUM(3-IP,II) =  PSUM(IP,II)
                     PGAIN(3-IP,II) = PGAIN(IP,II)
 35                  CONTINUE
 40               CONTINUE
               END IF
            END IF
         IRNO = IROW
         CALL TABSY ('WRIT', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID, PDIFF,
     *      PSUM, PGAIN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT SY TABLE'
            GO TO 990
            END IF
 50      CONTINUE
      CALL TABSY ('CLOS', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF, TIME,
     *   TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID, PDIFF, PSUM, PGAIN,
     *   IRET)
      CALL TABSY ('CLOS', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF, TIME,
     *   TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID, PDIFF, PSUM, PGAIN,
     *   IRET)
C                                       CD tables
      CALL CDINI ('READ', BUFF1, DISKIN, CNO, CDIVER, CATBLK, LUN1,
     *   IRNO, KOLS, NUMV, NUMANT, NUMPOL, NUMIF, RDATE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT CD TABLE'
         GO TO 990
         END IF
      CALL CDINI ('WRIT', BUFF2, DISKIN, CNO, CDOVER, CATBLK, LUN2,
     *   IRNO, KOLS, NUMV, NUMANT, NUMPOL, NUMIF, RDATE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT CD TABLE'
         GO TO 990
         END IF
      NROW = BUFF1(5)
      DO 100 IROW = 1,NROW
         IRNO = IROW
         CALL TABCD ('READ', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      ANTNO, SUBA, FREQID, TCAL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT CD TABLE'
            GO TO 990
            END IF
         IF (DOANT(ANTNO)) THEN
            IF (OPTYPE(:1).NE.'A') THEN
               DO 80 IP = IP1,IP2
                  ND = 0
                  SD = 0.0
                  MD = 0
                  SOLD = 0.0
                  DO 60 I = 1,NA
                     II = IA(I)
                     IF ((TCAL(IP,II).NE.FBLANK) .AND.
     *                  (TCAL(IP,II).GT.0.0)) THEN
                        ND = ND + 1
                        SD = SD + TCAL(IP,II)
                        END IF
                     IF ((TCAL(IP+2,II).NE.FBLANK) .AND.
     *                  (TCAL(IP+2,II).GT.0.0)) THEN
                        MD = MD + 1
                        SOLD = SOLD + TCAL(IP+2,II)
                        END IF
 60                  CONTINUE
                  IF (ND.GT.0) THEN
                     SD = SD / ND
                  ELSE
                     SD = FBLANK
                     END IF
                  IF (MD.GT.0) THEN
                     SOLD = SOLD / MD
                  ELSE
                     SOLD = FBLANK
                     END IF
                  DO 70 I = 1,NB
                     II = IB(I)
                     IF (SWAP) THEN
                        TCAL(3-IP,II) = SD
                        TCAL(3-IP+2,II) = SOLD
                     ELSE
                        TCAL(IP,II) = SD
                        TCAL(IP+2,II) = SOLD
                        END IF
 70                  CONTINUE
 80               CONTINUE
C                                       all IFs move
            ELSE
               DO 90 IP = IP1,IP2
                  DO 85 II = 1,NUMIF
                     TCAL(3-IP,II) = TCAL(IP,II)
                     TCAL(3-IP+2,II) = TCAL(IP+2,II)
 85                  CONTINUE
 90               CONTINUE
               END IF
            END IF
         IRNO = IROW
         CALL TABCD ('WRIT', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      ANTNO, SUBA, FREQID, TCAL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT CD TABLE'
            GO TO 990
            END IF
 100     CONTINUE
      CALL TABCD ('CLOS', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF, ANTNO,
     *   SUBA, FREQID, TCAL, IRET)
      CALL TABCD ('CLOS', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF, ANTNO,
     *   SUBA, FREQID, TCAL, IRET)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYCOSY: ERROR',I4,1X,A)
      END
      SUBROUTINE TYCOTY (IRET)
C-----------------------------------------------------------------------
C   Routine to average TY values over some IFs and place in other IFs
C   Output:
C      IRET     I       Return error code  0 => ok
C                          else quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRNO, KOLS(MAXTYC), NUMV(MAXTYC), NUMPOL, NUMIF, SOURID,
     *   ANTNO, SUBA, FREQID, IROW, NROW, ND, NS, II, IP, I
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF), SD, SS
      INCLUDE 'TYCOP.INC'
C-----------------------------------------------------------------------
C                                       TY table
      CALL TYINI ('READ', BUFF1, DISKIN, CNO, IVER, CATBLK, LUN1, IRNO,
     *   KOLS, NUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT TY TABLE'
         GO TO 990
         END IF
      IF (NUMPOL.EQ.1) THEN
         IF ((OPTYPE.NE.'R') .AND. (OPTYPE.NE.' ')) THEN
            MSGTXT = '1 POLARIZATION ONLY - OPTYPE=''' // OPTYPE //
     *         ' ILLEGAL'
            IRET = 10
            GO TO 990
            END IF
         IP2 = 1
         END IF
      CALL TYINI ('WRIT', BUFF2, DISKIN, CNO, OVER, CATBLK, LUN2, IRNO,
     *   KOLS, NUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT TY TABLE'
         GO TO 990
         END IF
      NROW = BUFF1(5)
      DO 50 IROW = 1,NROW
         IRNO = IROW
         CALL TABTY ('READ', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT TY TABLE'
            GO TO 990
            END IF
         IF (DOANT(ANTNO)) THEN
            IF (OPTYPE(:1).NE.'A') THEN
               DO 30 IP = IP1,IP2
                  ND = 0
                  NS = 0
                  SD = 0.0
                  SS = 0.0
                  DO 10 I = 1,NA
                     II = IA(I)
                     IF ((TSYS(IP,II).NE.FBLANK) .AND.
     *                  (TSYS(IP,II).GT.0.0))THEN
                        ND = ND + 1
                        SD = SD + TSYS(IP,II)
                        END IF
                     IF ((TANT(IP,II).NE.FBLANK) .AND.
     *                  (TANT(IP,II).GT.0.0)) THEN
                        NS = NS + 1
                        SS = SS + TANT(IP,II)
                        END IF
 10                  CONTINUE
                  IF (ND.GT.0) THEN
                     SD = SD / ND
                  ELSE
                     SD = FBLANK
                     END IF
                  IF (NS.GT.0) THEN
                     SS = SS / NS
                  ELSE
                     SS = FBLANK
                     END IF
                  DO 20 I = 1,NB
                     II = IB(I)
                     IF (SWAP) THEN
                        TSYS(3-IP,II) = SD
                        TANT(3-IP,II) = SS
                     ELSE
                        TSYS(IP,II) = SD
                        TANT(IP,II) = SS
                        END IF
 20                  CONTINUE
 30               CONTINUE
C                                       all IFs transfer
            ELSE
               DO 40 IP = IP1,IP2
                  DO 35 II = 1,NUMIF
                     TSYS(3-IP,II) = TSYS(IP,II)
                     TANT(3-IP,II) =  TANT(IP,II)
 35                  CONTINUE
 40               CONTINUE
               END IF
            END IF
         IRNO = IROW
         CALL TABTY ('WRIT', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT TY TABLE'
            GO TO 990
            END IF
 50      CONTINUE
      CALL TABTY ('CLOS', BUFF2, IRNO, KOLS, NUMV, NUMPOL, NUMIF, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT, IRET)
      CALL TABTY ('CLOS', BUFF1, IRNO, KOLS, NUMV, NUMPOL, NUMIF, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYCOTY: ERROR',I4,1X,A)
      END
