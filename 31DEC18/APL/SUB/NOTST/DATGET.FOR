      SUBROUTINE DATGET (RPARM, VIS, TIMLST, IERR)
C-----------------------------------------------------------------------
C! Reads, selects, calibrates and edits data.
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2004, 2007, 2009, 2015, 2017-2018
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
C   Reads next selected data record. Applies calibration and editing.
C   Inputs from common /SELCAL/:
C      INXRNO       I    Current INDEX file record number.
C                        If .LT. 0 then there is no index file.
C      NINDEX       I    Number of entries in the index table
C      NXKOLS(7)    I    Pointer array for index records.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      NANTSL       I    Number of antennas selected/excluded in ANTENS
C                        0 = All included.
C      DOAWNT       L    If .TRUE. then antennas in ANTENS included.
C                        If .FALSE. then excluded.
C      SUBARR       I    Subarray number desired, 0=>any.
C      NSOUWD       I    Number of sources specified.
C      DOSWNT       L    If true sources specified are included
C                        else excluded.
C      SOUWAN(30)   I    List or source numbers from source file.
C      DOACOR       L    If true, pass autocorrelations.
C      DOXCOR       L    If true, pass crosscorrelations.
C      TRANSL       L    If true, translate data to requested stokes.
C      JADR(2,4)    I    Table to translate input data to output vis.
C      TSTART       R    Start time in days.
C      TEND         R    End time in days.
C      UVRA(2)      R    UV range (wavelength squared)
C      NPRMIN       I    No. random parameters in the input data
C      NRPARM       I    No. random parameters in output data.
C      LRECIN       I    Length of input record in words.
C      KLOCSU       I    Source number pointer in input data.
C      KLOCWT       I    Weight pointer for compressed data.
C      NDECMP       I    Number of entries in DECMP
C      DECMP    I(2,*)   (1,*) = number of packed correlator values
C                        (2,*) = 0-rel offset in vis data.
C                        (from beginning of vis data NOT ran. parms.)
C      DOFQSL       L    If true, FREQSEL random parameter is present
C      FRQSEL       I    FQ entry to pass.
C   Input/output:
C      TIMLST       R    Time of last record
C   Output:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      IERR       I    Return code, 0=OK, else UVDISK error number.
C   Output to common /SELCAL/:
C      FSTVS3       I    Actual vis record number
C      FSTVIS       I    First word pointer of current buffer.
C      LSTVIS       I    Last word pointer of current buffer.
C      CURSOU       I    Current source number.
C     MRC 89/Sep/19: Ensure IERR is reset to 0.
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      RPARM(*), VIS(3,*), TIMLST
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMAX, NIO, BIND, IBUFF(1024), IA1, IA2, IRET, TIMKOL,
     *   INTKOL, SOUKOL, SUBKOL, FRSKOL, LSTKOL, FRQKOL, RECI(MAXNXC),
     *   IUBIND, CURSUB, THSOUR, CMPNT, CMPNT2, FREQRP, NREAD, BO, I,
     *   LIMT, J, ISUBA, JJ, ICORID
      LOGICAL   DROP, T, F, MATCH, MAT1, MAT2
      REAL      RECORD(MAXNXC), TIME, UVDIS, TMINT
      INTEGER   TBUFSZ
      PARAMETER (TBUFSZ = MAXCIF * 3)
      REAL      TBUFF(TBUFSZ)
      SAVE      TBUFF, RECORD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DPDC.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (IBUFF, NXBUFF)
      EQUIVALENCE (NXKOLS, TIMKOL), (NXKOLS(2), INTKOL),
     *   (NXKOLS(3), SOUKOL), (NXKOLS(4), SUBKOL), (NXKOLS(5), FRSKOL),
     *   (NXKOLS(6), LSTKOL), (NXKOLS(7), FRQKOL)
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
      IERR = 0
      TIME = -1.0E10
      IF (TYPUVD.GT.0) DOACOR = .TRUE.
 10   FSTVIS = FSTVIS + LRECIN
      RECNO3 = RECNO3 + 1
      IF (FSTVIS.GT.LSTVIS) THEN
C                                       Check expansion buffer size
         IF ((LRECIN.GT.TBUFSZ) .AND. ISCMP) THEN
            MSGTXT = 'DATGET: DATA EXPANSION BUFFER IS TOO SMALL'
            IERR = 5
            GO TO 990
            END IF
C                                       Read next buffer load.
         CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Check for end of scan
         IF (NIO.LE.0) GO TO 200
            FSTVIS = BIND
            LSTVIS = FSTVIS + (NIO-1) * LRECIN
C                                       See which records selected;
C                                       UBUFF(BIND)='INDE' => not wanted
C                                       Check freq random parm
            IF (DOFQSL .AND. (FRQSEL.GT.0)) THEN
               BIND = FSTVIS
      INCLUDE 'INCS:ZVND.INC'
               DO 15 I = 1,NIO
                  FREQRP = UBUFF(BIND+KLOCFQ)
                  IF (FREQRP.NE.FRQSEL) UBUFF(BIND) = FBLANK
                  BIND = BIND + LRECIN
 15               CONTINUE
               END IF
C                                       Check timerange
            BIND = FSTVIS
      INCLUDE 'INCS:ZVND.INC'
            DO 20 I = 1,NIO
               TIME = UBUFF(BIND+ILOCT)
               IF ((TIME.LT.TSTART) .OR. (TIME.GT.TEND))
     *            UBUFF(BIND) = FBLANK
               BIND = BIND + LRECIN
 20            CONTINUE
C                                       Check subarray
            BIND = FSTVIS
            IF (SUBARR.GT.0) THEN
      INCLUDE 'INCS:ZVND.INC'
               DO 40 I = 1,NIO
                  IF (ILOCB.GE.0) THEN
                     IA1 = UBUFF(BIND+ILOCB) + 0.1
                     CURSUB = 1.5 + 100.0 * (UBUFF(BIND+ILOCB) - IA1)
                  ELSE
                     CURSUB = UBUFF(BIND+ILOCSA) + 0.1
                     END IF
                  IF (CURSUB.NE.SUBARR) UBUFF(BIND) = FBLANK
                  BIND = BIND + LRECIN
 40               CONTINUE
               END IF
C                                       Check autocorrelations or
C                                       crosscorrelations
            BIND = FSTVIS
            IF ((.NOT.DOACOR) .OR. (.NOT.DOXCOR)) THEN
               DO 45 I = 1,NIO
                  IF (ILOCB.GE.0) THEN
                     IA1 = UBUFF(BIND+ILOCB) / 256.0 + 0.1
                     IA2 = UBUFF(BIND+ILOCB) - 256 * IA1 + 0.1
                  ELSE
                     IA1 = UBUFF(BIND+ILOCA1) + 0.1
                     IA2 = UBUFF(BIND+ILOCA2) + 0.1
                     END IF
                  IF ((.NOT.DOACOR) .AND. (IA1.EQ.IA2))
     *               UBUFF(BIND) = FBLANK
                  IF ((.NOT.DOXCOR) .AND. (IA1.NE.IA2))
     *               UBUFF(BIND) = FBLANK
                  BIND = BIND + LRECIN
 45               CONTINUE
               END IF
C                                       Check antennas
            BIND = FSTVIS
            IF (NANTSL.GT.0) THEN
               DO 60 I = 1,NIO
                  IF (ILOCB.GE.0) THEN
                     IA1 = UBUFF(BIND+ILOCB) / 256.0 + 0.1
                     IA2 = UBUFF(BIND+ILOCB) - 256 * IA1 + 0.1
                  ELSE
                     IA1 = UBUFF(BIND+ILOCA1) + 0.1
                     IA2 = UBUFF(BIND+ILOCA2) + 0.1
                     END IF
                  MAT1 = F
                  MAT2 = F
                  DO 50  J = 1,NANTSL
                     MAT1 = MAT1 .OR. (IA1.EQ.ABS(ANTENS(J)))
                     MAT2 = MAT2 .OR. (IA2.EQ.ABS(ANTENS(J)))
 50                  CONTINUE
C                                       Check for match selected.
                  IF (DOAWNT .AND. (MAT1.AND.MAT2)) GO TO 55
C                                       Check for match excluded
                  IF (.NOT.DOAWNT .AND. (MAT1.OR.MAT2))
     *               UBUFF(BIND) = FBLANK
C                                       If inclusion flag
                  IF (DOAWNT) UBUFF(BIND) = FBLANK
 55               BIND = BIND + LRECIN
 60               CONTINUE
               END IF
C                                       Check source
            BIND = FSTVIS
            IF ((NSOUWD.GT.0) .AND. (KLOCSU.GE.0)) THEN
               DO 100 I = 1,NIO
                  THSOUR = UBUFF(BIND+KLOCSU) + 0.5
                  IF ((THSOUR.EQ.CURSOU) .OR. (CURSOU.EQ.0)) GO TO 90
                  IF ((NSOUWD.EQ.1) .AND. (THSOUR.NE.CURSOU)) THEN
                     UBUFF(BIND) = FBLANK
                     GO TO 90
                     END IF
C                                       loop thru list
                  DO 70 J = 1,NSOUWD
                     IF (THSOUR.EQ.SOUWAN(J)) GO TO 80
 70                  CONTINUE
C                                       Not in list
                  IF (.NOT.DOSWNT) GO TO 90
                     UBUFF(BIND) = FBLANK
                     GO TO 90
C                                       In list
 80               IF (DOSWNT) GO TO 90
                     UBUFF(BIND) = FBLANK
 90               BIND = BIND + LRECIN
 100              CONTINUE
               END IF
         END IF
C                                       Check if wanted
      FSTVS3 = FSTVS3 + 1
      IF (UBUFF(FSTVIS).EQ.FBLANK) GO TO 10
      BIND = FSTVIS
      TIME = UBUFF(BIND+ILOCT)
      IF (ILOCB.GE.0) THEN
         IA1 = UBUFF(BIND+ILOCB) / 256.0 + 0.1
         IA2 = UBUFF(BIND+ILOCB) - 256 * IA1 + 0.1
         JJ = UBUFF(BIND+ILOCB) + 0.01
         ISUBA = 1.1 + 100.0 * (UBUFF(BIND+ILOCB) - JJ)
      ELSE
         IA1 = UBUFF(BIND+ILOCA1) + 0.1
         IA2 = UBUFF(BIND+ILOCA2) + 0.1
         ISUBA = UBUFF(BIND+ILOCSA) + 0.1
         END IF
C                                       Correlation id.
      IF (ILOCID.GE.0) THEN
         ICORID = UBUFF(BIND+ILOCID) + 0.1
      ELSE
         ICORID = 1
         END IF
C                                       Set integration time for
C                                       rate smearing correction:
C                                       default of 1 sec.
      IF (DXTIME.LE.0.0) THEN
         IF (ILOCIT.GE.0) THEN
            TMINT = UBUFF(BIND+ILOCIT) / 86400.0
         ELSE
            TMINT = 1.0 / 86400.0
            END IF
      ELSE
         TMINT = DXTIME
         END IF
      DORSM = (DXTIME.GE.0.0)
C                                       See if time order important
      IF (((DOCAL) .OR. (DOBL) .OR. (DOBAND.GT.1) .OR. (DOFLAG)) .AND.
     *   (TIMORD)) THEN
C                                       Trap aginst going backwards in
C                                       time.
         IF (TIME.LT.TIMLST) GO TO 10
         IF (TIME.GT.TEND) GO TO 200
         TIMLST = TIME
         END IF
C                                       Copy to output
      DO 110 I = 1,NRPARM
         RPARM(I) = UBUFF(BIND+I-1)
 110     CONTINUE
      IF (ISCMP) THEN
C                                       Compressed data - decompress
         DO 120 I = 1,NDECMP
            CMPNT = BIND + NPRMIN + DECMP(2,I)
            CMPNT2 = 1 + DECMP(2,I) * 3
            CALL ZUVXPN (DECMP(1,I), UBUFF(CMPNT), UBUFF(BIND+KLOCWT),
     *         TBUFF(CMPNT2))
 120         CONTINUE
C                                       Do flagging
         DROP = F
         IF (DOUVRA) THEN
            CALL DATUVR (RPARM, TBUFF, DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
         IF (DOFLAG) THEN
            CALL DATFLG (RPARM, TBUFF, DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
C                                       Calibrate
         IF (DOCAL .OR. DOBL) THEN
            CALL DATCAL (IA1, IA2, ISUBA, ICORID, TIME, TMINT, TBUFF,
     *         DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
C                                       Convolve with smoothing function
         IF (DOSMTH.EQ.1) THEN
            CALL SMOSP (TBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Bandpass correction
         IF (DOBAND.GT.0) THEN
            CALL DATBND (TIME, IA1, IA2, TBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Polarization correction
         IF (DOPOL.GT.0) THEN
            CALL DATPOL (IA1, IA2, TIME, TBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE IF (TRXYQU.GT.0) THEN
            CALL DATPAR (TIME, ISUBA, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Convolve with smoothing function
         IF (DOSMTH.EQ.2) THEN
            CALL SMOSP (TBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         NUMAX = 3
C                                       Get data in correct Stokes form.
         IF (TRXYQU.GT.0) THEN
            CALL DXYGET (TBUFF, NUMAX, IA1, IA2, VIS, DROP)
         ELSE
            CALL DGGET (TBUFF, NUMAX, TRANSL, MVIS, JADR, SELFAC, ALLWT,
     *         VIS, DROP)
            END IF
         IF (DROP) GO TO 10
C                                       Uncompressed data
      ELSE
C                                       Do flagging
         DROP = F
         IF (DOUVRA) THEN
            CALL DATUVR (RPARM, UBUFF(BIND+NPRMIN), DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
         IF (DOFLAG) THEN
            CALL DATFLG (RPARM, UBUFF(BIND+NPRMIN), DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
C                                       Calibrate
         IF (DOCAL .OR. DOBL) THEN
            CALL DATCAL (IA1, IA2, ISUBA, ICORID, TIME, TMINT,
     *         UBUFF(BIND+NPRMIN), DROP, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (DROP) GO TO 10
            END IF
C                                       Convolve with smoothing function
         IF (DOSMTH.EQ.1) THEN
            CALL SMOSP (UBUFF(BIND+NPRMIN), IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Bandpass correction
         IF (DOBAND.GT.0) THEN
            CALL DATBND (TIME, IA1, IA2, UBUFF(BIND+NPRMIN), IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Polarization correction
         IF (DOPOL.GT.0) THEN
            CALL DATPOL (IA1, IA2, TIME, UBUFF(BIND+NPRMIN), IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE IF (TRXYQU.GT.0) THEN
            CALL DATPAR (TIME, ISUBA, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Convolve with smoothing function
         IF (DOSMTH.EQ.2) THEN
            CALL SMOSP (UBUFF(BIND+NPRMIN), IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         NUMAX = CATUV(KINAX)
C                                       Get data in correct Stokes form.
         IF (TRXYQU.GT.0) THEN
            CALL DXYGET (UBUFF(BIND+NPRMIN), NUMAX, IA1, IA2, VIS, DROP)
         ELSE
            CALL DGGET (UBUFF(BIND+NPRMIN), NUMAX, TRANSL, MVIS, JADR,
     *         SELFAC, ALLWT, VIS, DROP)
            END IF
         IF (DROP) GO TO 10
         END IF
      GO TO 999
C                                       New scan.
C                                       Find next in INDEX file.
 200  IERR = 4
C                                       Check if INDEX file used.
      IF (INXRNO.LE.0) GO TO 999
      IF (INXRNO.GT.NINDEX) GO TO 999
C                                       See if time range exhausted.
      IF (TIME.GT.TEND) GO TO 999
C                                       Loop through records
      LIMT = INXRNO
      DO 260 I = LIMT,NINDEX
         INXRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, INXRNO, RECORD, IBUFF, IRET)
C                                       Check if flagged
         IF (IRET.LT.0) GO TO 260
C                                       Check error
         IF (IRET.GT.0) GO TO 999
         IERR = 0
C                                       Check time range.
         IF (TSTART.GT.(RECORD(TIMKOL)+RECORD(INTKOL))) GO TO 260
C                                       Check subarray
         IF ((SUBARR.GT.0) .AND. (RECI(SUBKOL).NE.SUBARR) .AND.
     *      (RECI(SUBKOL).GT.0)) GO TO 260
C                                       Check freq id
         IF ((FRQSEL.GT.0) .AND. (RECI(FRQKOL).NE.FRQSEL) .AND.
     *      (RECI(FRQKOL).GT.0)) GO TO 260
C                                       Set values
         FSTVIS = RECI(FRSKOL)
         LSTVIS = RECI(LSTKOL)
         IF (LSTVIS.LT.FSTVIS) GO TO 260
         CURSOU = RECI(SOUKOL)
C                                       See if all sources selected.
         IF (NSOUWD.LE.0) GO TO 300
C                                       Search source lists
         DO 240 J = 1,NSOUWD
            MATCH = RECI(SOUKOL) .EQ. SOUWAN(J)
            IF (MATCH) GO TO 250
 240        CONTINUE
C                                       See if found match
 250     IF ((MATCH.AND.DOSWNT) .OR. ((.NOT.MATCH).AND.(.NOT.DOSWNT)))
     *    GO TO 300
 260     CONTINUE
C                                       No more data selected.
      IERR = 4
      GO TO 999
C                                       Init I/O to uvfile
 300  INXRNO = INXRNO + 1
      NREAD = LSTVIS - FSTVIS + 1
      FSTRED = FSTVIS - 1
      RECNO3 = FSTRED
      NREAD3 = NREAD
      FSTRD3 = FSTRED
      FSTVS3 = FSTVIS - 1
      LSTVS3 = LSTVIS
      FSTVIS = 0
      LSTVIS = 0
      BIND = 0
      DOUVIN = T
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FSTRED, LRECIN, BIND,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      LENBU3 = BIND
      IF (IERR.LE.0) GO TO 10
         WRITE (MSGTXT,1300) IERR
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('DATGET: ERROR',I3,' READING UV DATA')
 1300 FORMAT ('DATGET: ERROR',I3,' INITIALIZING I/O TO UV DATA')
      END
