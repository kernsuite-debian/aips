LOCAL INCLUDE 'PCAVG.INC'
C                                                          Include DSPF
C                                       Local include for PCAVG
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2)
      CHARACTER NAMEIN*12, CLAIN*6
      REAL      XSIN, XDISIN, XINVER, SOLINT
      INTEGER   DISKIN, SEQIN, CNOIN, INVER, OUTVER, BUFFER(512)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XINVER, SOLINT
      COMMON /CHRCOM/ NAMEIN, CLAIN
      COMMON /INFOLS/ DISKIN, SEQIN, CNOIN, INVER, OUTVER, BUFFER
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
      PROGRAM PCAVG
C-----------------------------------------------------------------------
C! time average a PC table
C# UV Calibration EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2017
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
C   Task PCAVG sorts a PC table into time-antenna order and then
C   writes a new table with time averaging.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAMEIN        File name to be imaged
C      INCLASS        CLAIN         File class to be imaged
C      INSEQ          SEQIN         File sequence number
C      INDISK         DISKIN        Disk volume on which file resides
C      INVERS         INVER         Input version
C      SOLINT         SOLINT        Averaging time
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'PCAVG.INC'
      DATA PRGM /'PCAVG '/
C-----------------------------------------------------------------------
C                                       get inputs, ...
      CALL PCAVGI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (SOLINT.GT.0.0) THEN
         CALL PCAVGR (IRET)
      ELSE
         CALL PCAVGF (IRET)
         END IF
      IF (IRET.NE.0) GO TO 990
C                                       Do history
      CALL PCAVHI
C                                       close down
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE PCAVGI (PRGM, IRET)
C-----------------------------------------------------------------------
C   PCAVGI gets the inputs for PCAVG.
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
      INTEGER   NPARM, IERR, IROUND
      INCLUDE 'PCAVG.INC'
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
      NPARM = 9
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
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
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
 1000 FORMAT ('PCAVGI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I5,' DISK=',
     *   I3,' USID=',I5)
 1016 FORMAT (A12,'.',A6,'.',I5,' DISK=',I3,' USID=',I5,' NOT FOUND')
 1020 FORMAT ('PCAVGI: ERROR',I3,' READING CATBLK ')
      END
      SUBROUTINE PCAVGR (IRET)
C-----------------------------------------------------------------------
C   PCAVGR sorts the input table if needed and does the averaging to an
C   output table
C   Output:
C      IRET     I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCAVG.INC'
      INCLUDE 'PCDATA.INC'
      INTEGER   I, ISUB, PCLUN, KEY(2,2), KEYSUB(2,2), NXVER, ANTNUM,
     *   FREQID, IROW, ISCAN, NSCAN, NUMPC, NXLUN, SOUNUM, PC2LUN,
     *   OUBUFF(512), LANT, LSOUR, NAVG, LIF, LP, LCH, NACABC, LSUB,
     *   LFREQ, NAREAL(2,MAXTON,MAXIF), NASTAT(2,4,MAXIF)
      LOGICAL   GOOD
      REAL      FKEY(2,2), AVREAL(2,MAXTON,MAXIF), AVSTAT(2,4,MAXIF),
     *   AVIMAG(2,MAXTON,MAXIF), AVRATE(2,MAXTON,MAXIF), AVTIMI,
     *   STIMES(2000)
      DOUBLE PRECISION LTIME, AVFREQ(2,MAXTON,MAXIF), AVTIME, AVCABC
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB, KEY /4*1, 4*0/
      DATA PCLUN, PC2LUN, NXLUN /43,44,45/
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
      CALL PCINI ('WRIT', OUBUFF, DISKIN, CNOIN, OUTVER, CATBLK, PC2LUN,
     *   OUTROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT PC TABLE'
         GO TO 990
         END IF
      OUBUFF(43) = PCBUFF(43)
      OUBUFF(44) = PCBUFF(44)
C                                       scan list
      CALL FNDEXT ('NX', CATBLK, NXVER)
      IF (NXVER.LE.0) THEN
         NSCAN = 0
         MSGTXT = 'NO INDEX TABLE SO NO SCAN BREAKS'
         CALL MSGWRT (6)
      ELSE
         ISUB = 0
         CALL GETNX (NXLUN, DISKIN, CNOIN, CATBLK, ISUB, BUFFER,
     *      NSCAN, STIMES)
         IF (NSCAN.LE.0) THEN
            MSGTXT = 'INDEX TABLE TROUBLES, SO NO SCAN BREAKS'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       prepare for averaging
      MSGTXT = 'Averaging input table'
      CALL MSGWRT (2)
      NUMPC = PCBUFF(5)
      LANT = 0
      LSOUR = 0
      LTIME = -1000.
      SOLINT = SOLINT / 86400.0
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
            END IF
C                                       do an average
         IF ((SOUNUM.NE.LSOUR) .OR. (ANTNUM.NE.LANT) .OR.
     *      (IROW.GT.NUMPC) .OR. (TIME.GT.LTIME+SOLINT) .OR.
     *      (TIME.GT.STIMES(ISCAN+1))) THEN
            IF (NAVG.GT.0) THEN
               IF (TIME.GT.STIMES(ISCAN+1)) ISCAN = ISCAN + 1
               GOOD = .FALSE.
               DO 40 LIF = 1,PCNIF
                  DO 30 LP = 1,PCNPOL
                     DO 10 LCH = 1,NUMTON
                        IF (NAREAL(LP,LCH,LIF).GT.0) THEN
                           AVREAL(LP,LCH,LIF) = AVREAL(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           AVIMAG(LP,LCH,LIF) = AVIMAG(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           AVRATE(LP,LCH,LIF) = AVRATE(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           GOOD = .TRUE.
                        ELSE
                           AVREAL(LP,LCH,LIF) = FBLANK
                           AVIMAG(LP,LCH,LIF) = FBLANK
                           AVRATE(LP,LCH,LIF) = FBLANK
                           END IF
                        AVFREQ(LP,LCH,LIF) = AVFREQ(LP,LCH,LIF) / NAVG
 10                     CONTINUE
                     DO 20 LCH = 1,4
                        IF (NASTAT(LP,LCH,LIF).GT.0) THEN
                           AVSTAT(LP,LCH,LIF) = AVSTAT(LP,LCH,LIF) /
     *                        NASTAT(LP,LCH,LIF)
                        ELSE
                           AVSTAT(LP,LCH,LIF) = FBLANK
                           END IF
 20                     CONTINUE
 30                  CONTINUE
 40               CONTINUE
               AVTIME = AVTIME / NAVG
               IF (NACABC.GT.0) THEN
                  AVCABC = AVCABC / NACABC
               ELSE
                  AVCABC = DBLANK
                  END IF
               IF (GOOD) THEN
                  CALL TABPC ('WRIT', OUBUFF, OUTROW, PCKOLS, PCNUMV,
     *               PCNPOL, AVTIME, AVTIMI, LSOUR, LANT, LSUB, LFREQ,
     *               AVCABC, AVSTAT, AVFREQ, AVREAL, AVIMAG, AVRATE,
     *               IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING PC TABLE'
                     GO TO 990
                     END IF
                  END IF
               IF (IROW.GT.NUMPC) GO TO 900
               END IF
C                                       zero summing arrays
            NAVG = 1
            LSOUR = SOUNUM
            LANT = ANTNUM
            LSUB = ISUB
            LFREQ = FREQID
            DO 140 LIF = 1,PCNIF
               DO 130 LP = 1,PCNPOL
                  DO 110 LCH = 1,NUMTON
                     IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                        NAREAL(LP,LCH,LIF) = 1
                        AVREAL(LP,LCH,LIF) = PCREAL(LP,LCH,LIF)
                        AVIMAG(LP,LCH,LIF) = PCIMAG(LP,LCH,LIF)
                        AVRATE(LP,LCH,LIF) = PCRATE(LP,LCH,LIF)
                     ELSE
                        NAREAL(LP,LCH,LIF) = 0
                        AVREAL(LP,LCH,LIF) = 0.0
                        AVIMAG(LP,LCH,LIF) = 0.0
                        AVRATE(LP,LCH,LIF) = 0.0
                        END IF
                     AVFREQ(LP,LCH,LIF) = PCFREQ(LP,LCH,LIF)
 110                 CONTINUE
                  DO 120 LCH = 1,4
                     IF (STATE(LP,LCH,LIF).NE.FBLANK) THEN
                        NASTAT(LP,LCH,LIF) = 1
                        AVSTAT(LP,LCH,LIF) = STATE(LP,LCH,LIF)
                     ELSE
                        AVSTAT(LP,LCH,LIF) = 0.0
                        NASTAT(LP,LCH,LIF) = 0
                        END IF
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
            AVTIME = TIME
            LTIME = TIME
            AVTIMI = TIMINT
            IF (CABCAL.NE.DBLANK) THEN
               AVCABC = CABCAL
               NACABC = 1
            ELSE
               NACABC = 0
               AVCABC = 0.0D0
               END IF
C                                       add in this record
         ELSE
            DO 240 LIF = 1,PCNIF
               DO 230 LP = 1,PCNPOL
                  DO 210 LCH = 1,NUMTON
                     IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                        NAREAL(LP,LCH,LIF) = NAREAL(LP,LCH,LIF) + 1
                        AVREAL(LP,LCH,LIF) = AVREAL(LP,LCH,LIF) +
     *                     PCREAL(LP,LCH,LIF)
                        AVIMAG(LP,LCH,LIF) = AVIMAG(LP,LCH,LIF) +
     *                     PCIMAG(LP,LCH,LIF)
                        AVRATE(LP,LCH,LIF) = AVRATE(LP,LCH,LIF) +
     *                     PCRATE(LP,LCH,LIF)
                        END IF
                     AVFREQ(LP,LCH,LIF) = AVFREQ(LP,LCH,LIF) +
     *                  PCFREQ(LP,LCH,LIF)
 210                 CONTINUE
                  DO 220 LCH = 1,4
                     IF (STATE(LP,LCH,LIF).NE.FBLANK) THEN
                        NASTAT(LP,LCH,LIF) = NASTAT(LP,LCH,LIF) + 1
                        AVSTAT(LP,LCH,LIF) = AVSTAT(LP,LCH,LIF) +
     *                     STATE(LP,LCH,LIF)
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
            NAVG = NAVG + 1
            AVTIME = AVTIME + TIME
            AVTIMI = AVTIMI + TIMINT
            IF (CABCAL.NE.DBLANK) THEN
               AVCABC = AVCABC + CABCAL
               NACABC = NACABC + 1
               END IF
            END IF
 300     CONTINUE
C                                       done - close up files
 900  CALL TABPC ('CLOS', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL,STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, IRET)
      CALL TABPC ('CLOS', OUBUFF, OUTROW, PCKOLS, PCNUMV, PCNPOL,
     *   AVTIME, AVTIMI, LSOUR, LANT, LSUB, LFREQ, AVCABC, AVSTAT,
     *   AVFREQ, AVREAL, AVIMAG, AVRATE, IRET)
      SOLINT = SOLINT * 86400.0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCAVGR: ERROR',I5,' ON ',A)
      END
      SUBROUTINE PCAVGF (IRET)
C-----------------------------------------------------------------------
C   PCAVGF sorts the input table if needed and does the averaging to an
C   output table using stict intervals within each scan
C   Output:
C      IRET     I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PCAVG.INC'
      INCLUDE 'PCDATA.INC'
      INTEGER   I, ISUB, PCLUN, KEY(2,2), KEYSUB(2,2), NXVER, ANTNUM,
     *   FREQID, IROW, ISCAN, NSCAN, NACABC, LSUB, LFREQ, NUMPC, NXLUN,
     *   SOUNUM, PC2LUN, OUBUFF(512), LANT, LSOUR, NAVG, LIF, LP, LCH,
     *   NAREAL(2,MAXTON,MAXIF), NASTAT(2,4,MAXIF), LSCAN, IINTR, LINTR
      LOGICAL   GOOD
      REAL      FKEY(2,2), AVREAL(2,MAXTON,MAXIF), AVSTAT(2,4,MAXIF),
     *   AVIMAG(2,MAXTON,MAXIF), AVRATE(2,MAXTON,MAXIF), AVTIMI,
     *   STIMES(2,2000), START, STOP, TINT
      DOUBLE PRECISION LTIME, AVFREQ(2,MAXTON,MAXIF), AVTIME, AVCABC
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB, KEY /4*1, 4*0/
      DATA PCLUN, PC2LUN, NXLUN /43,44,45/
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
      CALL PCINI ('WRIT', OUBUFF, DISKIN, CNOIN, OUTVER, CATBLK, PC2LUN,
     *   OUTROW, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT PC TABLE'
         GO TO 990
         END IF
      OUBUFF(43) = PCBUFF(43)
      OUBUFF(44) = PCBUFF(44)
C                                       scan list
      CALL FNDEXT ('NX', CATBLK, NXVER)
      IF (NXVER.LE.0) THEN
         NSCAN = 0
         MSGTXT = 'NO INDEX TABLE SO NO SCAN BREAKS'
         CALL MSGWRT (6)
         NSCAN = 1
         STIMES(1,1) = 0.0
         STIMES(2,1) = 1000.
      ELSE
         ISUB = 0
         CALL GETNDX (NXLUN, DISKIN, CNOIN, CATBLK, ISUB, BUFFER,
     *      NSCAN, STIMES)
         IF (NSCAN.LE.0) THEN
            MSGTXT = 'INDEX TABLE TROUBLES, SO NO SCAN BREAKS'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       prepare for averaging
      MSGTXT = 'Averaging input table'
      CALL MSGWRT (2)
      NUMPC = PCBUFF(5)
      LANT = 0
      LSOUR = 0
      LTIME = -1000.
      SOLINT = -SOLINT / 86400.0
      NAVG = 0
      LSCAN = 0
      LINTR = 0
      STOP = -1.0
      DO 400 IROW = 1,NUMPC+1
         IF (IROW.LE.NUMPC) THEN
            PCROW = IROW
            CALL TABPC ('READ', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL,
     *         TIME, TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL,
     *         STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING PC TABLE'
               GO TO 990
               END IF
            IF (IRET.LT.0) GO TO 400
C                                       which scan
            DO 10 ISCAN = 1,NSCAN
               IF ((TIME.GE.STIMES(1,ISCAN)) .AND.
     *            (TIME.LE.STIMES(2,ISCAN))) GO TO 15
 10            CONTINUE
C                                       not in scan - omit
            GO TO 400
 15         IF (ISCAN.NE.LSCAN) THEN
               I = (STIMES(2,ISCAN) - STIMES(1,ISCAN)) / SOLINT + 0.5
               I = MAX (I, 1)
               TINT = (STIMES(2,ISCAN) - STIMES(1,ISCAN)) / I
               END IF
C                                       which interval - start stop
            IINTR = (TIME - STIMES(1,ISCAN)) / TINT + 1.0
            END IF
C                                       do an average
         IF ((SOUNUM.NE.LSOUR) .OR. (ANTNUM.NE.LANT) .OR.
     *      (IROW.GT.NUMPC) .OR. (ISCAN.NE.LSCAN) .OR. (IINTR.NE.LINTR)
     *      .OR. (TIME.GT.STOP)) THEN
            IF (NAVG.GT.0) THEN
               GOOD = .FALSE.
               DO 140 LIF = 1,PCNIF
                  DO 130 LP = 1,PCNPOL
                     DO 110 LCH = 1,NUMTON
                        IF (NAREAL(LP,LCH,LIF).GT.0) THEN
                           AVREAL(LP,LCH,LIF) = AVREAL(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           AVIMAG(LP,LCH,LIF) = AVIMAG(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           AVRATE(LP,LCH,LIF) = AVRATE(LP,LCH,LIF) /
     *                        NAREAL(LP,LCH,LIF)
                           GOOD = .TRUE.
                        ELSE
                           AVREAL(LP,LCH,LIF) = FBLANK
                           AVIMAG(LP,LCH,LIF) = FBLANK
                           AVRATE(LP,LCH,LIF) = FBLANK
                           END IF
                        AVFREQ(LP,LCH,LIF) = AVFREQ(LP,LCH,LIF) / NAVG
 110                    CONTINUE
                     DO 120 LCH = 1,4
                        IF (NASTAT(LP,LCH,LIF).GT.0) THEN
                           AVSTAT(LP,LCH,LIF) = AVSTAT(LP,LCH,LIF) /
     *                        NASTAT(LP,LCH,LIF)
                        ELSE
                           AVSTAT(LP,LCH,LIF) = FBLANK
                           END IF
 120                    CONTINUE
 130                 CONTINUE
 140              CONTINUE
               AVTIME = (START + STOP) / 2.0
               IF (NACABC.GT.0) THEN
                  AVCABC = AVCABC / NACABC
               ELSE
                  AVCABC = DBLANK
                  END IF
               IF (GOOD) THEN
                  CALL TABPC ('WRIT', OUBUFF, OUTROW, PCKOLS, PCNUMV,
     *               PCNPOL, AVTIME, AVTIMI, LSOUR, LANT, LSUB, LFREQ,
     *               AVCABC, AVSTAT, AVFREQ, AVREAL, AVIMAG, AVRATE,
     *               IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING PC TABLE'
                     GO TO 990
                     END IF
                  END IF
               IF (IROW.GT.NUMPC) GO TO 900
               END IF
C                                       zero summing arrays
            NAVG = 1
            LSOUR = SOUNUM
            LANT = ANTNUM
            LSUB = ISUB
            LFREQ = FREQID
            LSCAN = ISCAN
            LINTR = IINTR
            STOP = STIMES(1,LSCAN) + LINTR * TINT
            START = STOP - TINT
            DO 240 LIF = 1,PCNIF
               DO 230 LP = 1,PCNPOL
                  DO 210 LCH = 1,NUMTON
                     IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                        NAREAL(LP,LCH,LIF) = 1
                        AVREAL(LP,LCH,LIF) = PCREAL(LP,LCH,LIF)
                        AVIMAG(LP,LCH,LIF) = PCIMAG(LP,LCH,LIF)
                        AVRATE(LP,LCH,LIF) = PCRATE(LP,LCH,LIF)
                     ELSE
                        NAREAL(LP,LCH,LIF) = 0
                        AVREAL(LP,LCH,LIF) = 0.0
                        AVIMAG(LP,LCH,LIF) = 0.0
                        AVRATE(LP,LCH,LIF) = 0.0
                        END IF
                     AVFREQ(LP,LCH,LIF) = PCFREQ(LP,LCH,LIF)
 210                 CONTINUE
                  DO 220 LCH = 1,4
                     IF (STATE(LP,LCH,LIF).NE.FBLANK) THEN
                        NASTAT(LP,LCH,LIF) = 1
                        AVSTAT(LP,LCH,LIF) = STATE(LP,LCH,LIF)
                     ELSE
                        AVSTAT(LP,LCH,LIF) = 0.0
                        NASTAT(LP,LCH,LIF) = 0
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
            AVTIME = TIME
            LTIME = TIME
            AVTIMI = TIMINT
            IF (CABCAL.NE.DBLANK) THEN
               AVCABC = CABCAL
               NACABC = 1
            ELSE
               NACABC = 0
               AVCABC = 0.0D0
               END IF
C                                       add in this record
         ELSE
            DO 340 LIF = 1,PCNIF
               DO 330 LP = 1,PCNPOL
                  DO 310 LCH = 1,NUMTON
                     IF (PCREAL(LP,LCH,LIF).NE.FBLANK) THEN
                        NAREAL(LP,LCH,LIF) = NAREAL(LP,LCH,LIF) + 1
                        AVREAL(LP,LCH,LIF) = AVREAL(LP,LCH,LIF) +
     *                     PCREAL(LP,LCH,LIF)
                        AVIMAG(LP,LCH,LIF) = AVIMAG(LP,LCH,LIF) +
     *                     PCIMAG(LP,LCH,LIF)
                        AVRATE(LP,LCH,LIF) = AVRATE(LP,LCH,LIF) +
     *                     PCRATE(LP,LCH,LIF)
                        END IF
                     AVFREQ(LP,LCH,LIF) = AVFREQ(LP,LCH,LIF) +
     *                  PCFREQ(LP,LCH,LIF)
 310                 CONTINUE
                  DO 320 LCH = 1,4
                     IF (STATE(LP,LCH,LIF).NE.FBLANK) THEN
                        NASTAT(LP,LCH,LIF) = NASTAT(LP,LCH,LIF) + 1
                        AVSTAT(LP,LCH,LIF) = AVSTAT(LP,LCH,LIF) +
     *                     STATE(LP,LCH,LIF)
                        END IF
 320                 CONTINUE
 330              CONTINUE
 340           CONTINUE
            NAVG = NAVG + 1
            AVTIME = AVTIME + TIME
            AVTIMI = AVTIMI + TIMINT
            IF (CABCAL.NE.DBLANK) THEN
               AVCABC = AVCABC + CABCAL
               NACABC = NACABC + 1
               END IF
            END IF
 400     CONTINUE
C                                       done - close up files
 900  CALL TABPC ('CLOS', PCBUFF, PCROW, PCKOLS, PCNUMV, PCNPOL, TIME,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, FREQID, CABCAL,STATE, PCFREQ,
     *   PCREAL, PCIMAG, PCRATE, IRET)
      CALL TABPC ('CLOS', OUBUFF, OUTROW, PCKOLS, PCNUMV, PCNPOL,
     *   AVTIME, AVTIMI, LSOUR, LANT, LSUB, LFREQ, AVCABC, AVSTAT,
     *   AVFREQ, AVREAL, AVIMAG, AVRATE, IRET)
      SOLINT = -SOLINT * 86400.0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCAVGF: ERROR',I5,' ON ',A)
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
      INCLUDE 'PCAVG.INC'
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
C                                       solint
      IF (SOLINT.GT.0.0) THEN
         WRITE (HILINE,1010) TSKNAM, SOLINT
      ELSE
         WRITE (HILINE,1011) TSKNAM, -SOLINT
         END IF
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
C                                       Close HI file
 100  CALL HICLOS (HLUNI, .TRUE., BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A9,2X,A8)
 1010 FORMAT (A6,'SOLINT=',F6.1,'  / general averaging time in seconds')
 1011 FORMAT (A6,'SOLINT=',F6.1,
     *   '  / interval averaging time in seconds')
 1015 FORMAT (A6,'OUTVERS=',I5,'  / output PC table version')
 1020 FORMAT (A6,'/  wrote',I8,' records in the output PC table')
      END
      SUBROUTINE GETNDX (LUN, DISK, CNO, CATBLK, ISUB, BUFFER, NOSCAN,
     *   TSCAN)
C-----------------------------------------------------------------------
C   GETNX reads the NX table and makes a list of scan start and stop
C   Inputs:
C      LUN      I      LUN to use
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   Header
C      ISUB     I      Limit to subarray ISUB - 0 -> all
C   Outputs
C      BUFFER   I(*)   Scratch buffer
C      NOSCAN   I      Number of times in TSCAN
C      TSCAN    R(*)   Time of scan boundaries
C-----------------------------------------------------------------------
      INTEGER   LUN, DISK, CNO, CATBLK(256), ISUB, BUFFER(*), NOSCAN
      REAL      TSCAN(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC), IERR,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, NROW, IROW
      REAL      TIME, DTIME, TEPS
      INCLUDE 'INCS:DMSG.INC'
C                                       0.1 seconds
      DATA TEPS /1.2e-6/
C-----------------------------------------------------------------------
      VER = 1
      CALL NDXINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INDEX TABLE'
         GO TO 900
         END IF
      NROW = BUFFER(5)
      NOSCAN = 0
      DO 100 IROW = 1,NROW
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INDEX TABLE'
            GO TO 900
            END IF
         IF ((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR)) THEN
            NOSCAN = NOSCAN + 1
            DTIME = DTIME / 2.0 + TEPS
            TSCAN(1,NOSCAN) = TIME - DTIME
            TSCAN(2,NOSCAN) = TIME + DTIME
            END IF
 100     CONTINUE
C
 900  IF (IERR.NE.0) THEN
         CALL MSGWRT (6)
         NOSCAN = 0
         END IF
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETNDX ERROR',I4,' ON ',A)
      END
