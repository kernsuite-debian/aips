LOCAL INCLUDE 'PCRMS.INC'
C                                                          Include DSPF
C                                       Local include for PCRMS
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XOUTF(12)
      CHARACTER NAMEIN*12, CLAIN*6, OUTFIL*48
      REAL      XSIN, XDISIN, XINVER, APARM(10), PRTLEV, BADD(10)
      INTEGER   DISKIN, SEQIN, CNOIN, INVER, OUTVER, BUFFER(512), NTFLAG
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XINVER, APARM,
     *   PRTLEV, XOUTF, BADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, OUTFIL
      COMMON /INFOLS/ DISKIN, SEQIN, CNOIN, INVER, OUTVER, NTFLAG,
     *   BUFFER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C                                                          End DSPF
LOCAL END
LOCAL INCLUDE 'PCDATA.INC'
      INCLUDE 'INCS:PPCV.INC'
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2,MAXTON,MAXIF)
      INTEGER   PCNPOL, PCNIF, NUMTON, PCBUFF(512), PCNUMV(MAXPCC),
     *   PCKOLS(MAXPCC), PCROW, OUTROW
      REAL      TIMINT, STATE(2,4,MAXIF), PCREAL(2,MAXTON,MAXIF),
     *   PCIMAG(2,MAXTON,MAXIF), PCRATE(2,MAXTON,MAXIF)
      COMMON /PCDATA/ PCFREQ, TIME, CABCAL, STATE, PCREAL, PCIMAG,
     *   PCRATE, PCBUFF, TIMINT, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON,
     *   PCROW, OUTROW
LOCAL END
LOCAL INCLUDE 'FGDATA.INC'
      INTEGER   FGCHAN(2,MAXIF,MAXANT), FLAG
      REAL      FGAMP(2,MAXIF,MAXANT), FGAMPR(2,MAXIF,MAXANT),
     *   FGRMS(2,MAXIF,MAXANT), FGRMSR(2,MAXIF,MAXANT)
      COMMON /FGDATA/ FGAMP, FGAMPR, FGRMS, FGRMSR, FGCHAN, FLAG
LOCAL END
      PROGRAM PCRMS
C-----------------------------------------------------------------------
C! get statistics of PC table
C# UV Calibration EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2017-2018
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
C   Task PCRMS sorts a PC table into time-antenna order and then
C   writes a new table with time averaging.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAMEIN        File name to be imaged
C      INCLASS        CLAIN         File class to be imaged
C      INSEQ          SEQIN         File sequence number
C      INDISK         DISKIN        Disk volume on which file resides
C      INVERS         INVER         Input version
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'PCRMS.INC'
      DATA PRGM /'PCRMS '/
C-----------------------------------------------------------------------
C                                       get inputs, ...
      CALL PCRMSI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL PCRMS1 (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL PCRMS2 (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       do flagging
      IF (APARM(4).LE.0.0) THEN
         CALL PCRMSF (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Do history
         CALL PCAVHI
         END IF
C                                       close down
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE PCRMSI (PRGM, IRET)
C-----------------------------------------------------------------------
C   PCRMSI gets the inputs for PCRMS.
C   Inputs:
C      PRGM   C*6   Task name
C   Output:
C      ONEIF  L     T => input has <= 1 IF
C      IRET   I     Error code: 0 ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   NPARM, IERR, IROUND, I
      INCLUDE 'PCRMS.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 41
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IRET = 5
      IF (APARM(1).LE.0.0) APARM(1) = 5.0
      IF (APARM(2).LE.0.0) APARM(2) = 5.0
C                                       Crunch input parameters.
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XOUTF, OUTFIL)
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.5) THEN
            WRITE (MSGTXT,1015) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *         NLUSER
         ELSE
            WRITE (MSGTXT,1016) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
            END IF
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCRMSI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I5,' DISK=',
     *   I3,' USID=',I5)
 1016 FORMAT (A12,'.',A6,'.',I5,' DISK=',I3,' USID=',I5,' NOT FOUND')
 1020 FORMAT ('PCRMSI: ERROR',I3,' READING CATBLK ')
      END
      SUBROUTINE PCRMS1 (IRET)
C-----------------------------------------------------------------------
C   PCRMS1 sorts the input table if needed and does the averaging to
C   get and report statistics
C   Output:
C      IRET     I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCRMS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'FGDATA.INC'
      INTEGER   I, ISUB, PCLUN, KEY(2,2), KEYSUB(2,2), ANTNUM, FREQID,
     *   IROW, ISCAN, NUMPC, SOUNUM, J,LANT, LSOUR, NAVG, LIF, LP, LCH,
     *   EDGE, K, KK, NADEVI(2,MAXTON,MAXIF,2), NVDEVI(2,MAXIF,2),
     *   CHDEVI(10), VADEVI(10), CHD(6), VAD(6), JJ, MAXNV, MAXREC,
     *   NCDEVI(2,MAXIF,2), NXDEVI(2,MAXTON,MAXIF,2), MAXCV
      REAL      FKEY(2,2), AVG, AVGS, VALUE(2,MAXTON,MAXIF,2), V,
     *   PCDELY(2,MAXIF), PCPHAS(2,MAXIF), ERDELY(2,MAXIF), MAXD, MAXX,
     *   ERPHAS(2,MAXIF), WEIGHT(2,MAXIF), TCREAL(2,MAXTON,MAXIF),
     *   TCIMAG(2,MAXTON,MAXIF)
      DOUBLE PRECISION LTIME
      CHARACTER TYPE(2)*20
      INCLUDE 'INCS:PSTD.INC'
      DATA TYPE /'Residual amplitude', 'Residual phase'/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB, KEY /4*1, 4*0/
      DATA PCLUN /43/
C-----------------------------------------------------------------------
C                                       open PC table
      INVER = XINVER + 0.1
      CALL FNDEXT ('PC', CATBLK, I)
      IF (INVER.LE.0) INVER = I
      IF (INVER.GT.I) INVER = I
      OUTVER = I + 1
      CALL PCINI ('READ', PCBUFF, DISKIN, CNOIN, INVER, CATBLK, PCLUN,
     *   PCROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT PC TABLE'
         GO TO 990
         END IF
      XINVER = INVER
C                                       sort to output PC table
      CALL TABIO ('CLOS', 0, PCROW, PCBUFF, PCBUFF, IRET)
      IF ((PCBUFF(43).NE.4) .OR. (PCBUFF(44).NE.1)) THEN
         MSGTXT = 'Sorting input table'
         CALL MSGWRT (2)
         KEY(1,1) = 4
         KEY(1,2) = 1
         CALL TABSRT (DISKIN, CNOIN, 'PC', INVER, INVER, KEY, KEYSUB,
     *      FKEY, PCBUFF, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SORTING PC TABLE'
            GO TO 990
            END IF
         END IF
      CALL PCINI ('READ', PCBUFF, DISKIN, CNOIN, INVER, CATBLK, PCLUN,
     *   PCROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPENING INPUT PC TABLE'
         GO TO 990
         END IF
C                                       prepare for averaging
      EDGE = MIN (7, NUMTON / 4 - 1)
      FLAG = MAX (1, NUMTON / 10)
      MSGTXT = 'Reading input table for divergent channels'
      CALL MSGWRT (2)
      NUMPC = PCBUFF(5)
      LANT = 0
      LSOUR = 0
      LTIME = -1000.
      NAVG = 0
      ISCAN = 1
      DO 300 IROW = 1,NUMPC+1
         IF (IROW.LE.NUMPC) THEN
            PCROW = IROW
            CALL TABPC ('READ', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL,
     *         TIME, TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL,
     *         STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
               GO TO 990
               END IF
            IF (IRET.LT.0) GO TO 300
         ELSE
            ANTNUM = -1000
            END IF
C                                       report this antenna
         IF (LANT.NE.ANTNUM) THEN
            IF (LANT.GT.0) THEN
               MAXNV = 0
               MAXCV = 0
               DO 30 I = 1,2
                  DO 25 LIF = 1,PCNIF
                     DO 20 LP = 1,PCNPOL
                        MAXNV = MAX (MAXNV, NVDEVI(LP,LIF,I))
                        MAXCV = MAX (MAXCV, NCDEVI(LP,LIF,I))
 20                     CONTINUE
 25                  CONTINUE
 30               CONTINUE
C                                       deviant channels
               DO 55 I = 1,2
                  WRITE (MSGTXT,1010) LANT, TYPE(I), ' deviant'
                  IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                  DO 50 LIF = 1,PCNIF
                     DO 45 LP = 1,PCNPOL
C                        V = 0.10 * NVDEVI(LP,LIF,I)
                        V = 0.20 * MAXREC
                        K = 0
C                        IF (NVDEVI(LP,LIF,I).LT.MAXNV/5) GO TO 43
C                                       find strongest
                        DO 40 LCH = 1,NUMTON
                           IF (NADEVI(LP,LCH,LIF,I).GT.V) THEN
                              K = K + 1
                              CHDEVI(K) = LCH
                              VADEVI(K) = NADEVI(LP,LCH,LIF,I)
                              END IF
 40                        CONTINUE

C                                       sort
                        DO 42 J = 1,MIN(K,6)
                           MAXD = 0
                           JJ = 0
                           DO 41 LCH = 1,K
                              IF (VADEVI(LCH).GT.MAXD) THEN
                                 JJ = LCH
                                 MAXD = VADEVI(LCH)
                                 END IF
 41                           CONTINUE
                           CHD(J) = CHDEVI(JJ)
                           VAD(J) = VADEVI(JJ)
                           VADEVI(JJ) = -1
 42                        CONTINUE
                        JJ = MIN (K, 6)
                        IF (JJ.GT.0) THEN
                           WRITE (MSGTXT,1052) LP, LIF, (CHD(J), VAD(J),
     *                        J = 1,JJ)
                           IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                        ELSE
                           WRITE (MSGTXT,1052) LP, LIF
                           IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                           END IF
 45                     CONTINUE
 50                  CONTINUE
 55               CONTINUE
C                                       stick-out channels
               DO 75 I = 1,2
                  WRITE (MSGTXT,1010) LANT, TYPE(I), ' stick out'
                  IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                  DO 70 LIF = 1,PCNIF
                     DO 65 LP = 1,PCNPOL
C                       V = 0.10 * NCDEVI(LP,LIF,I)
                        V = 0.20 * MAXREC
                        K = 0
                        CHD(1) = 0
C                       IF (NCDEVI(LP,LIF,I).LT.MAXCV/5) GO TO 63
C                                       find strongest
                        DO 60 LCH = 1,NUMTON
                           IF (NXDEVI(LP,LCH,LIF,I).GT.V) THEN
                              K = K + 1
                              CHDEVI(K) = LCH
                              VADEVI(K) = NXDEVI(LP,LCH,LIF,I)
                              END IF
 60                        CONTINUE
C                                       sort
                        DO 62 J = 1,MIN(K,6)
                           MAXD = 0
                           JJ = 0
                           DO 61 LCH = 1,K
                              IF (VADEVI(LCH).GT.MAXD) THEN
                                 JJ = LCH
                                 MAXD = VADEVI(LCH)
                                 END IF
 61                           CONTINUE
                           CHD(J) = CHDEVI(JJ)
                           VAD(J) = VADEVI(JJ)
                           VADEVI(JJ) = -1
 62                        CONTINUE
                        JJ = MIN (K, 6)
                        IF (JJ.GT.0) THEN
                           FGCHAN(LP,LIF,LANT) = CHD(1)
                           WRITE (MSGTXT,1052) LP, LIF, (CHD(J), VAD(J),
     *                        J = 1,JJ)
                           IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                        ELSE
                           WRITE (MSGTXT,1052) LP, LIF
                           IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                           END IF
 65                     CONTINUE
 70                  CONTINUE
 75               CONTINUE
               IF (ANTNUM.LT.0) GO TO 900
               END IF
            WRITE (MSGTXT,1075) ANTNUM
            IF (PRTLEV.LE.0.0) CALL MSGWRT (2)
C                                       zero summing arrays
            I = 2 * 2 * MAXTON * MAXIF
            CALL FILL (I, 0, NADEVI)
            CALL FILL (I, 0, NXDEVI)
            I = 2 * 2 * MAXIF
            CALL FILL (I, 0, NVDEVI)
            CALL FILL (I, 0, NCDEVI)
            MAXREC = 0
            END IF
C                                       flag edges
         LANT = ANTNUM
         DO 130 LIF = 1,PCNIF
            DO 120 LP = 1,PCNPOL
               DO 110 K = 1,FLAG
                  PCREAL(LP,K,LIF) = FBLANK
                  PCREAL(LP,NUMTON+1-K,LIF) = FBLANK
                  PCIMAG(LP,K,LIF) = FBLANK
                  PCIMAG(LP,NUMTON+1-K,LIF) = FBLANK
 110              CONTINUE
               DO 115 K = 1,NUMTON
                  TCREAL(LP,K,LIF) = PCREAL(LP,K,LIF)
                  TCIMAG(LP,K,LIF) = PCIMAG(LP,K,LIF)
 115              CONTINUE
 120           CONTINUE
 130        CONTINUE
C                                       solve delay
         CALL PCFITR (PCNPOL, PCNIF, 1, NUMTON, PCFREQ, TCREAL, TCIMAG,
     *      0, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT, IRET)
         MAXREC = MAXREC + 1
C                                       residual amp and phase
         DO 160 LIF = 1,PCNIF
            DO 150 LP = 1,PCNPOL
               DO 140 LCH = 1,NUMTON
                  IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                     VALUE(LP,LCH,LIF,1) = SQRT (PCREAL(LP,LCH,LIF)**2 +
     *                  PCIMAG(LP,LCH,LIF)**2)
                  ELSE
                     VALUE(LP,LCH,LIF,1) = FBLANK
                     END IF
                  IF (TCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                     VALUE(LP,LCH,LIF,2) = ATAN2 (TCIMAG(LP,LCH,LIF),
     *                  TCREAL(LP,LCH,LIF)) * RAD2DG
                  ELSE
                     VALUE(LP,LCH,LIF,2) = FBLANK
                     END IF
 140              CONTINUE
 150           CONTINUE
 160        CONTINUE
C                                       statistics this record
         DO 290 I = 1,2
            DO 240 LIF = 1,PCNIF
               DO 230 LP = 1,PCNPOL
                  J = 0
                  AVG = 0.0
                  AVGS = 0.0
                  DO 210 LCH = 1,NUMTON
                     IF (VALUE(LP,LCH,LIF,I).NE.FBLANK) THEN
                        J = J + 1
                        AVG = AVG + VALUE(LP,LCH,LIF,I)
                        AVGS = AVGS + VALUE(LP,LCH,LIF,I)**2
                        END IF
 210                 CONTINUE
                  IF (J.GT.0) THEN
                     AVG = AVG / J
                     AVGS = AVGS / J - AVG * AVG
                     AVGS = SQRT (MAX (0.0, AVGS))
                     MAXD = 2.0*AVGS
                     MAXX = MAXD
                     KK = 0
                     K = 0
                     DO 220 LCH = 1+EDGE,NUMTON-EDGE
                        IF (VALUE(LP,LCH,LIF,I).NE.FBLANK) THEN
                           V = ABS (VALUE(LP,LCH,LIF,I) - AVG)
                           IF (V.GT.MAXD) THEN
                              K = LCH
                              MAXD = V
                              END IF
                           IF ((VALUE(LP,LCH-1,LIF,I).NE.FBLANK) .AND.
     *                        (VALUE(LP,LCH+1,LIF,I).NE.FBLANK)) THEN
                              V = ABS (VALUE(LP,LCH,LIF,I) -
     *                           (VALUE(LP,LCH-1,LIF,I) +
     *                           VALUE(LP,LCH+1,LIF,I)) / 2.0)
                              IF (V.GT.MAXX) THEN
                                 KK = LCH
                                 MAXX = V
                                 END IF
                              END IF
                           END IF
 220                    CONTINUE
                     IF (K.GT.0) THEN
                        NADEVI(LP,K,LIF,I) = NADEVI(LP,K,LIF,I) + 1
                        NVDEVI(LP,LIF,I) = NVDEVI(LP,LIF,I) + 1
                        END IF
                     IF (KK.GT.0) THEN
                        NXDEVI(LP,KK,LIF,I) = NXDEVI(LP,KK,LIF,I) + 1
                        NCDEVI(LP,LIF,I) = NCDEVI(LP,LIF,I) + 1
                        END IF
                     END IF
 230              CONTINUE
 240           CONTINUE
 290        CONTINUE
 300     CONTINUE
C                                       done - close up files
 900  CALL TABPC ('CLOS', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCRMS1: ERROR',I5,' ON ',A)
 1010 FORMAT ('Report on antenna',I3,' type ',A,A)
 1052 FORMAT (I1,I3,6(I4,I6))
 1075 FORMAT ('Starting on antenna',I4)
      END
      SUBROUTINE PCRMS2 (IRET)
C-----------------------------------------------------------------------
C   PCRMS2 sorts the input table if needed and does the averaging to
C   get and report statistics
C   Output:
C      IRET     I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCRMS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'FGDATA.INC'
      INTEGER   I, ISUB, PCLUN, KEY(2,2), KEYSUB(2,2), ANTNUM, FREQID,
     *   IROW, ISCAN, NUMPC, SOUNUM, J,LANT, LSOUR, NAVG, LIF, LP, LCH,
     *   NASTAT(2,MAXIF,2), EDGE, K, TXLUN, TXIND, TXANT, I2
      REAL      FKEY(2,2), RMSTAT(2,MAXIF,2), MINDEV(2,MAXIF,2),
     *   AVSTAT(2,MAXIF,2), RMS, RMSS, AVG, AVGS, MAXRMS(2,MAXIF,2),
     *   MAXDEV(2,MAXIF,2), RMSTST(2,MAXIF,2), AVSTST(2,MAXIF,2),
     *   VALUE(2,MAXTON,MAXIF,2), PCDELY(2,MAXIF), PCPHAS(2,MAXIF),
     *   ERDELY(2,MAXIF), ERPHAS(2,MAXIF), WEIGHT(2,MAXIF), MAXD,
     *   MAXX, MINRMS(2,MAXIF,2), PRNTV(2,4,2), TCREAL(2,MAXTON,MAXIF),
     *   TCIMAG(2,MAXTON,MAXIF)
      DOUBLE PRECISION LTIME
      CHARACTER TYPE(2)*20, LINE*148
      LOGICAL   DOPHAS
      INCLUDE 'INCS:PSTD.INC'
      DATA TYPE /'Amplitude', 'Residual phase'/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB, KEY /4*1, 4*0/
      DATA PCLUN, TXLUN /43, 3/
C-----------------------------------------------------------------------
      IF (OUTFIL.NE.' ') THEN
         TXANT = APARM(3) + 0.1
         IF (TXANT.LE.0) OUTFIL = ' '
         END IF
      IF (OUTFIL.NE.' ') THEN
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTFIL, .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TEXT FILE'
            GO TO 990
            END IF
         END IF
      DOPHAS = APARM(5).GT.0.0
      I2 = 1
      IF (DOPHAS) I2 = 2
C                                       open PC table
      INVER = XINVER + 0.1
      CALL FNDEXT ('PC', CATBLK, I)
      IF (INVER.LE.0) INVER = I
      IF (INVER.GT.I) INVER = I
      OUTVER = I + 1
      CALL PCINI ('READ', PCBUFF, DISKIN, CNOIN, INVER, CATBLK, PCLUN,
     *   PCROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT PC TABLE'
         GO TO 990
         END IF
      XINVER = INVER
C                                       prepare for averaging
      EDGE = MIN (7, NUMTON / 4 - 1)
      FLAG = MAX (1, NUMTON / 10)
      MSGTXT = 'Reading input table over again for amplitudes, rmses'
      CALL MSGWRT (2)
      NUMPC = PCBUFF(5)
      LANT = 0
      LSOUR = 0
      LTIME = -1000.
      NAVG = 0
      ISCAN = 1
      DO 300 IROW = 1,NUMPC+1
         IF (IROW.LE.NUMPC) THEN
            PCROW = IROW
            CALL TABPC ('READ', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL,
     *         TIME, TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL,
     *         STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
               GO TO 990
               END IF
            IF (IRET.LT.0) GO TO 300
         ELSE
            ANTNUM = -1000
            END IF
C                                       report this antenna
         IF (LANT.NE.ANTNUM) THEN
            IF (LANT.GT.0) THEN
               DO 30 I = 1,I2
                  IF (PRTLEV.GT.0.0) THEN
                     WRITE (MSGTXT,1010) LANT, TYPE(I)
                     CALL MSGWRT (4)
                     WRITE (MSGTXT,1015)
                     CALL MSGWRT (4)
                     WRITE (MSGTXT,1016)
                     CALL MSGWRT (4)
                     END IF
                  DO 25 LIF = 1,PCNIF
                     DO 20 LP = 1,PCNPOL
                        IF (NASTAT(LP,LIF,I).GT.0) THEN
                           RMS = RMSTAT(LP,LIF,I) / NASTAT(LP,LIF,I)
                           RMSS = RMSTST(LP,LIF,I) / NASTAT(LP,LIF,I)
                           RMSS = SQRT (MAX (0.0, RMSS - RMS*RMS))
                           AVG = AVSTAT(LP,LIF,I) / NASTAT(LP,LIF,I)
                           AVGS = AVSTST(LP,LIF,I) / NASTAT(LP,LIF,I)
                           AVGS = SQRT (MAX (0.0, AVGS - AVG*AVG))
                           MAXD = MAX (ABS (MAXDEV(LP,LIF,I)-AVG),
     *                        ABS(AVG-MINDEV(LP,LIF,I)))
                           MAXX = MAX (MAXRMS(LP,LIF,I)-RMS,
     *                        RMS-MINRMS(LP,LIF,I))
                           IF (I.EQ.2) THEN
                              WRITE (MSGTXT,1020) LP, LIF, AVG, AVGS,
     *                           MAXD, RMS, RMSS, MAXX
                           ELSE
                              FGAMP(LP,LIF,LANT) = AVG
                              FGAMPR(LP,LIF,LANT) = AVGS
                              FGRMS(LP,LIF,LANT) = RMS
                              FGRMSR(LP,LIF,LANT) = RMSS
                              WRITE (MSGTXT,1021) LP, LIF, AVG, AVGS,
     *                           MAXD, RMS, RMSS, MAXX
                              END IF
                           IF (PRTLEV.GT.0.0) CALL MSGWRT (4)
                           END IF
 20                     CONTINUE
 25                  CONTINUE
 30               CONTINUE
               IF (ANTNUM.LT.0) GO TO 900
               END IF
            WRITE (MSGTXT,1030) ANTNUM
            IF (PRTLEV.LE.0.0) CALL MSGWRT (2)
C                                       zero summing arrays
            I = 2 * 2 * MAXIF
            CALL RFILL (I, 0.0, RMSTAT)
            CALL RFILL (I, 0.0, RMSTST)
            CALL RFILL (I, 0.0, AVSTAT)
            CALL RFILL (I, 0.0, AVSTST)
            CALL RFILL (I, -1.E6, MAXDEV)
            CALL RFILL (I, 0.0, MAXRMS)
            CALL RFILL (I, 1.E6, MINDEV)
            CALL RFILL (I, 1.E6, MINRMS)
            CALL FILL (I, 0, NASTAT)
            END IF
C                                       flag edges
         LANT = ANTNUM
         DO 130 LIF = 1,PCNIF
            DO 120 LP = 1,PCNPOL
               DO 110 K = 1,FLAG
                  PCREAL(LP,K,LIF) = FBLANK
                  PCREAL(LP,NUMTON+1-K,LIF) = FBLANK
                  PCIMAG(LP,K,LIF) = FBLANK
                  PCIMAG(LP,NUMTON+1-K,LIF) = FBLANK
 110              CONTINUE
C                                       interior channel?
               IF (FGCHAN(LP,LIF,ANTNUM).GT.0) THEN
                  K = FGCHAN(LP,LIF,ANTNUM)
                  PCREAL(LP,K,LIF) = FBLANK
                  PCIMAG(LP,K,LIF) = FBLANK
                  END IF
               IF (DOPHAS) THEN
                  DO 115 K = 1,NUMTON
                     TCREAL(LP,K,LIF) = PCREAL(LP,K,LIF)
                     TCIMAG(LP,K,LIF) = PCIMAG(LP,K,LIF)
 115                 CONTINUE
                  END IF
 120           CONTINUE
 130        CONTINUE
C                                       solve delay
         IF (DOPHAS) THEN
            CALL PCFITR (PCNPOL, PCNIF, 1, NUMTON, PCFREQ, TCREAL,
     *         TCIMAG, 0, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT,
     *         IRET)
            END IF
C                                       residual amp and phase
         DO 160 LIF = 1,PCNIF
            DO 150 LP = 1,PCNPOL
               DO 140 LCH = 1,NUMTON
                  IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                     VALUE(LP,LCH,LIF,1) = SQRT (PCREAL(LP,LCH,LIF)**2 +
     *                  PCIMAG(LP,LCH,LIF)**2)
                  ELSE
                     VALUE(LP,LCH,LIF,1) = FBLANK
                     END IF
                  IF ((DOPHAS) .AND. (TCREAL(LP,LCH,LIF).NE.FBLANK))
     *               THEN
                     VALUE(LP,LCH,LIF,2) = ATAN2 (TCIMAG(LP,LCH,LIF),
     *                  TCREAL(LP,LCH,LIF)) * RAD2DG
                  ELSE
                     VALUE(LP,LCH,LIF,2) = FBLANK
                     END IF
 140              CONTINUE
 150           CONTINUE
 160        CONTINUE
C                                       statistics this record
         DO 290 I = 1,I2
            CALL RFILL (16, 0.0, PRNTV)
            DO 240 LIF = 1,PCNIF
               DO 230 LP = 1,PCNPOL
                  J = 0
                  AVG = 0.0
                  AVGS = 0.0
                  DO 210 LCH = 1,NUMTON
                     IF (VALUE(LP,LCH,LIF,I).NE.FBLANK) THEN
                        J = J + 1
                        AVG = AVG + VALUE(LP,LCH,LIF,I)
                        AVGS = AVGS + VALUE(LP,LCH,LIF,I)**2
                        END IF
 210                 CONTINUE
                  IF (J.GT.0) THEN
                     AVG = AVG / J
                     AVGS = AVGS / J - AVG * AVG
                     AVGS = SQRT (MAX (0.0, AVGS))
                     IF (LIF.LE.4) THEN
                        IF (I.EQ.1) THEN
                           IF ((AVG.LT.0.0) .OR. (AVG.GE.100.0) .OR.
     *                        (AVGS.LT.0.0) .OR.(AVGS.GE.100.0)) THEN
                              MSGTXT = 'WE ARE HERE'
                              END IF
                           PRNTV(LP,LIF,1) = AVG
                           PRNTV(LP,LIF,2) = AVGS
                           END IF
                        END IF
                     NASTAT(LP,LIF,I) = NASTAT(LP,LIF,I) + 1
                     RMSTAT(LP,LIF,I) = RMSTAT(LP,LIF,I) + AVGS
                     RMSTST(LP,LIF,I) = RMSTST(LP,LIF,I) + AVGS*AVGS
                     AVSTAT(LP,LIF,I) = AVSTAT(LP,LIF,I) + AVG
                     AVSTST(LP,LIF,I) = AVSTST(LP,LIF,I) + AVG*AVG
                     MAXRMS(LP,LIF,I) = MAX (AVGS, MAXRMS(LP,LIF,I))
                     MAXDEV(LP,LIF,I) = MAX (AVG, MAXDEV(LP,LIF,I))
                     MINRMS(LP,LIF,I) = MIN (AVGS, MINRMS(LP,LIF,I))
                     MINDEV(LP,LIF,I) = MIN (AVG, MINDEV(LP,LIF,I))
                     END IF
 230              CONTINUE
 240           CONTINUE
            IF ((I.EQ.1) .AND. (LANT.EQ.TXANT) .AND. (OUTFIL.NE.' '))
     *         THEN
               WRITE (LINE,1240) PRNTV
               CALL ZTXIO ('WRIT', TXLUN, TXIND, LINE, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE TEXT FILE'
                  GO TO 990
                  END IF
               END IF
 290        CONTINUE
 300     CONTINUE
C                                       done - close up files
 900  CALL TABPC ('CLOS', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, IRET)
      IF (OUTFIL.NE.' ') CALL ZTXCLS (TXLUN, TXIND, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCRMS2: ERROR',I5,' ON ',A)
 1010 FORMAT ('Report on antenna',I3,' type ',A,A)
 1015 FORMAT ('P IF',10X,'Amplitude',7X,12X,'RMS')
 1016 FORMAT (6X,' Avg',5X,' rms',5X,' peak',4X,' avg',5X,' rms',5X,
     *   ' peak')
 1020 FORMAT (I1,I3,2F9.4,F9.2,2F9.4,F9.2)
 1021 FORMAT (I1,I3,6F9.5)
 1030 FORMAT ('Starting on antenna',I4)
 1240 FORMAT (16F9.6)
      END
      SUBROUTINE PCRMSF (IRET)
C-----------------------------------------------------------------------
C   PCRMSF rereads the sorted input table and applies the flags
C   Output:
C      IRET     I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCRMS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'FGDATA.INC'
      INTEGER   I, LP, LIF, LA, LCH, IROW, LANT, NAFLAG(2,MAXIF,MAXANT),
     *   NRFLAG(2,MAXIF,MAXANT), NCFLAG(2,MAXIF,MAXANT), PCLUN, PCLUNO,
     *   PCBUFO(512), NUMPC, NV, ANTNUM, FREQID, ISUB, K, SOUNUM
      REAL      AV, AVS, V
      LOGICAL   DOIT
      DATA PCLUN, PCLUNO /43, 44/
C-----------------------------------------------------------------------
C                                       zero counters
      NTFLAG = 0
      I = 2 * MAXANT * MAXIF
      CALL FILL (I, 0, NAFLAG)
      CALL FILL (I, 0, NRFLAG)
      CALL FILL (I, 0, NCFLAG)
C                                       open up PC tables
      CALL PCINI ('READ', PCBUFF, DISKIN, CNOIN, INVER, CATBLK, PCLUN,
     *   PCROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPENING INPUT PC TABLE'
         GO TO 990
         END IF
      CALL PCINI ('WRIT', PCBUFO, DISKIN, CNOIN, OUTVER, CATBLK, PCLUNO,
     *   OUTROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT PC TABLE'
         GO TO 990
         END IF
C                                       control sort
      PCBUFO(43) = PCBUFF(43)
      PCBUFO(44) = PCBUFF(44)
C                                       read loop
      NUMPC = PCBUFF(5)
      LANT = 0
      DO 300 IROW = 1,NUMPC
         PCROW = IROW
         CALL TABPC ('READ', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE,
     *      PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
            GO TO 990
            END IF
         IF (IRET.LT.0) GO TO 300
         IF (ANTNUM.NE.LANT) THEN
            WRITE (MSGTXT,1001) ANTNUM
            CALL MSGWRT (2)
            END IF
         LANT = ANTNUM
         DO 80 LIF = 1,PCNIF
            DO 70 LP = 1,PCNPOL
C                                       flag edges
               IF (APARM(6).LE.0.0) THEN
                  DO 40 K = 1,FLAG
                     IF (PCREAL(LP,K,LIF).NE.FBLANK) NTFLAG = NTFLAG + 1
                     PCREAL(LP,K,LIF) = FBLANK
                     PCREAL(LP,NUMTON+1-K,LIF) = FBLANK
                     PCIMAG(LP,K,LIF) = FBLANK
                     PCIMAG(LP,NUMTON+1-K,LIF) = FBLANK
 40                  CONTINUE
                  END IF
C                                       interior channel?
               IF (FGCHAN(LP,LIF,ANTNUM).GT.0) THEN
                  K = FGCHAN(LP,LIF,ANTNUM)
                  IF (PCREAL(LP,K,LIF).NE.FBLANK) NTFLAG = NTFLAG + 1
                  PCREAL(LP,K,LIF) = FBLANK
                  PCIMAG(LP,K,LIF) = FBLANK
                  NCFLAG(LP,LIF,ANTNUM) = NCFLAG(LP,LIF,ANTNUM) + 1
                  END IF
 70            CONTINUE
 80         CONTINUE
C                                       residual amp and phase
         DO 160 LIF = 1,PCNIF
            DO 150 LP = 1,PCNPOL
               NV = 0
               AV = 0.0
               AVS = 0.0
               DO 110 LCH = FLAG+1,NUMTON-FLAG
                  IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                     V = SQRT (PCREAL(LP,LCH,LIF)**2 +
     *                  PCIMAG(LP,LCH,LIF)**2)
                     NV = NV + 1
                     AV = AV + V
                     AVS = AVS + V*V
                     END IF
 110              CONTINUE
               IF (NV.GT.0) THEN
                  IF ((LANT.EQ.1) .AND. (LP.EQ.1) .AND. (LIF.EQ.4)) THEN
                     MSGTXT = 'WE ARE HERE'
                     END IF
                  IF ((LANT.EQ.8) .AND. (LP.EQ.1) .AND.(LIF.EQ.2)) THEN
                     MSGTXT = 'WE ARE HERE'
                     END IF
                  AV = AV / NV
                  AVS = AVS / NV - AV * AV
                  AVS = SQRT (MAX (0.0, AVS))
                  DOIT = .FALSE.
                  V = ABS (AV - FGAMP(LP,LIF,ANTNUM))
                  IF (V.GT.APARM(1)*FGAMPR(LP,LIF,ANTNUM)) THEN
                     DOIT = .TRUE.
                     NAFLAG(LP,LIF,ANTNUM)= NAFLAG(LP,LIF,ANTNUM) + 1
                     END IF
                  V = ABS (AVS - FGRMS(LP,LIF,ANTNUM))
                  IF ((V.GT.APARM(2)*FGRMSR(LP,LIF,ANTNUM)) .OR.
     *               (AVS.LT.FGRMS(LP,LIF,ANTNUM)/25.0)) THEN
                     DOIT = .TRUE.
                     NRFLAG(LP,LIF,ANTNUM)= NRFLAG(LP,LIF,ANTNUM) + 1
                     END IF
                  IF (DOIT) THEN
                     DO 120 LCH = 1,NUMTON
                        IF (PCREAL(LP,LCH,LIF).NE.FBLANK) NTFLAG =
     *                     NTFLAG + 1
                        PCREAL(LP,LCH,LIF) = FBLANK
                        PCIMAG(LP,LCH,LIF) = FBLANK
 120                    CONTINUE
                     END IF
                  END IF
 150           CONTINUE
 160        CONTINUE
C                                       write output
         CALL TABPC ('WRIT', PCBUFO, OUTROW, PCKOLS, PCNUMV, PCNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE,
     *      PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING PC TABLE'
            GO TO 990
            END IF
 300     CONTINUE
C                                       close tables
      CALL TABPC ('CLOS', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, I)
      CALL TABPC ('CLOS', PCBUFO, OUTROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL, STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, I)
C                                       reports
      LA = PCNPOL * PCNIF * NUMTON * NUMPC
      MSGTXT = 'Summary of flags applied:'
      CALL MSGWRT (4)
      WRITE (MSGTXT,1300) NTFLAG, LA
      CALL MSGWRT (4)
      IF (APARM(6).LE.0.0) THEN
         WRITE (MSGTXT,1305) FLAG
      ELSE
         WRITE (MSGTXT,1306) FLAG
         END IF
      CALL MSGWRT (4)
      IF (PRTLEV.GE.0.0) THEN
         MSGTXT = 'Single interior channels flagged'
         CALL MSGWRT (4)
         WRITE (MSGTXT,1310)
         CALL MSGWRT (4)
C                                       channels
         DO 320 LA = 1,LANT
            DO 315 LIF = 1,PCNIF
               DO 310 LP = 1,PCNPOL
                  IF (FGCHAN(LP,LIF,LA).GT.0) THEN
                     WRITE (MSGTXT,1311) LP, LIF, LA, FGCHAN(LP,LIF,LA),
     *                  NCFLAG(LP,LIF,LA)
                     CALL MSGWRT (4)
                     END IF
 310              CONTINUE
 315           CONTINUE
 320        CONTINUE
C                                       whole IFs
         MSGTXT = 'Number of whole IFs flagged for bad average' //
     *      ' amp or rms'
         CALL MSGWRT (4)
         WRITE (MSGTXT,1320)
         CALL MSGWRT (4)
         DO 350 LA = 1,LANT
            DO 345 LIF = 1,PCNIF
               DO 340 LP = 1,PCNPOL
                  IF ((NAFLAG(LP,LIF,LA).GT.0) .OR.
     *               (NRFLAG(LP,LIF,LA).GT.0)) THEN
                     WRITE (MSGTXT,1321) LP, LIF, LA, NAFLAG(LP,LIF,LA),
     *                  APARM(1)*FGAMPR(LP,LIF,LA), NRFLAG(LP,LIF,LA),
     *                  APARM(2)*FGRMSR(LP,LIF,LA)
                     CALL MSGWRT (4)
                     END IF
 340              CONTINUE
 345           CONTINUE
 350        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCRMSF: ERROR',I4,' ON ',A)
 1001 FORMAT ('Reading input table, flagging, writing output',
     *   ' at antenna',I4)
 1300 FORMAT ('   Total samples flagged',I15,' of',I15)
 1305 FORMAT ('   Flagged',I3,' channels at edge of each SPW')
 1306 FORMAT ('   Ignored',I3,' channels at edge of each SPW',
     *   ' but not flagged')
 1310 FORMAT ('Pol  IF Ant  Chan     flagged samples')
 1311 FORMAT (I3,I4,I4,I6,I12)
 1320 FORMAT ('POL  IF   Ant   AMP flagged  deviat    RMS flagged',
     *   '  deviat')
 1321 FORMAT (I3,I4,I6,I14,F9.4,I14,F9.4)
      END
      SUBROUTINE PCAVHI
C-----------------------------------------------------------------------
C   PCAVHI adds to the history file of the input UV data set info on
C   what was flagged.  It then removes that flagging info from the
C   flag command file and from the master grid, when these are kept in
C   the image catalog.
C-----------------------------------------------------------------------
C
      CHARACTER HILINE*72, CTIME(2)*12
      INTEGER   HLUNI, IERR, ITIME(3), DATE(3)
      INCLUDE 'PCRMS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA HLUNI /28/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
      CALL HIOPEN (HLUNI, DISKIN, CNOIN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       output version
      WRITE (HILINE,1015) TSKNAM, OUTVER
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
C                                       records written
      WRITE (HILINE,1020) TSKNAM, OUTROW-1
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      MSGTXT = HILINE(7:)
      CALL MSGWRT (2)
C                                       total flags
      WRITE (HILINE,1030) TSKNAM, NTFLAG
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (HLUNI, .TRUE., BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A9,2X,A8)
 1015 FORMAT (A6,'OUTVERS=',I5,'  / output PC table version')
 1020 FORMAT (A6,'/  wrote',I8,' records in the output PC table')
 1030 FORMAT (A6,'/  flagged',I12,' samples in the output PC table')
      END
