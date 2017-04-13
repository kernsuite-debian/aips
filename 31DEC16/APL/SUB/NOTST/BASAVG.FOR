      SUBROUTINE BASAVG (NUMVIS, NUMBAS, ANTLAB, AMPVEC, DTYPE, DT,
     *   CHNSEL, DOSPEC, SCANV, TIME, NUSCAN, SCANUM, RPARM, VIS, AVTIM,
     *   AVU, AVV, AVW, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2009, 2015
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
C   Reads a uv data base and returns averages of amp, phase or the RMS
C   scatter for selected baselines.
C   Needs to be initialized by a call to UVGET.
C   The  order of the baselines returned in SCANV is defined by
C   the order in the array NATLAB.  All data specified  (channels, IFs)
C   are averaged but only one polarization is allowed.
C   UVW are scaled by FREQ/UVFREQ to be at the first IF/FREQID of the
C   data.
C   Inputs:
C     NUMVIS       I    Current visibility number
C     NUMBAS       I    Number of baselines to average.
C     ANTLAB(MXBASE,2)  I    The First (*,1) and second (*,2) antenna
C                      numbers of each baseline selected.
C     AMPVEC       L    If true do ampscalar averaging else vector.
C     DTYPE        I    Output type, 0 => amplitude, 1=> phase,
C                       2 => amplitude RMS.
C     DT           R    Averaging time in days
C     CHNSEL(3,10) I    Channel selection
C     DOSPEC       L    TRUE if spectral averaging to be done
C   Input/Output:
C     RPARM(20)    R    Random parameter array, first record of call.
C                      (1) = 'INDE' => don't use.
C                      (2) if single baseline averaged then RPARM
C                          reflects the data output.
C     VIS(3,*)     R    Visibility array, first record of call.
C   Outputs:
C     SCANV(MXBASE)R   The result for baselines in ANTLAB
C     TIME(8)      I    Time range, start, stop; days, hours, min, sec.
C                       Unless NUSCAN only first 4 values are set.
C     NUSCAN       L    True IF the first record in a new scan.
C     AVTIM        R    Average time (days) of output record
C     AVU          R    Average u of output record
C     AVV          R    Average v of output record
C     AVW          R    Average w of output record
C     IERR         I    Return code, 0 => OK, -1 => out of data,
C                       > 0 => failed.
C   Output to common in D/CSOU.INC
C     SNAME(4)     R    Source name (16 char. 4 / word.)
C     QUAL         I    Source qualifier.
C     CALCOD       R    Calibrator code 4 char.
C     FLUX(4,IF)   R    Total flux density I, Q, U, V pol, (Jy) each IF
C     FREQO(IF)    D    Frequency offset (Hz)
C   Note:   If the end of data is encountered (IERR=-1) then UVGET is
C   called with OPCODE='CLOS'.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   AMPVEC, NUSCAN, DONE1, GOTDAT, DOSPEC
      INTEGER   DTYPE, TIME(8), NUMBAS, IERR, I, JA1, JA2, SUNUM, JERR,
     *   ISLUN, IBASE, IVIS, COUNT(MXBASE), KVIS, SCANUM, CNTTIM,
     *   NUMFRQ, NUMPLZ, CHNSEL(3,10), ANTLAB(MXBASE,2), NUMVIS, IDAY,
     *   IVSCNT
      REAL      RPARM(20), VIS(3,*), DT, T1, AMP, SQRT, SUMTIM,
     *   WORK(MXBASE,4), TEMP, TEMP1, TEMP2, SCANV(MXBASE),
     *   SUMU, SUMV, SUMW, AVTIM, AVU, AVV, AVW, TLAST, CT, DTUTC
      DOUBLE PRECISION X8, UVSCAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      SAVE TLAST, DTUTC, IVSCNT
      DATA ISLUN /26/
C-----------------------------------------------------------------------
      IF (NUMVIS.EQ.0) THEN
         TLAST = -1.0
         DTUTC = DATUTC / 86400.0
         IVSCNT = 0
         END IF
C                                       See if first record read
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
      IERR = 0
C                                       Clear arrays
 10   CONTINUE
      DO 50 I = 1,NUMBAS
         COUNT(I) = 0
         WORK(I,1) = 0.0
         WORK(I,2) = 0.0
         WORK(I,3) = 0.0
         WORK(I,4) = 0.0
         SCANV(I) = FBLANK
 50      CONTINUE
      CNTTIM = 0
      SUMTIM = 0.0
      SUMU = 0.0
      SUMV = 0.0
      SUMW = 0.0
      AVTIM  = 0.0
      AVU = 0.0
      AVV = 0.0
      AVW = 0.0
C                                       Initialize time
      T1 = 1.0E10
C                                       Save scan number (0= no index)
      NUSCAN = SCANUM.NE.INXRNO
      SCANUM = INXRNO
      KVIS = (LREC-NRPARM) / 3
      NUMFRQ = CATBLK(KINAX+JLOCF)
      NUMPLZ = CATBLK(KINAX+JLOCS)
      UVSCAL = 1.0D0
      IF (UVFREQ.GT.0.0D0) UVSCAL = FREQ / UVFREQ
C                                       Loop reading data
 100     IF (.NOT.DONE1) THEN
            CALL UVGET ('READ', RPARM, VIS, IERR)
            IVSCNT = IVSCNT + 1
            END IF
         IF (IERR.GT.0) GO TO 999
         IF (IERR.EQ.-1) GO TO 500
         DONE1 = .FALSE.
C
         CT = RPARM(ILOCT+1) - DTUTC
C                                       Set up first time boundary
         IF (IVSCNT.EQ.1) THEN
            IDAY = CT
            X8 = (CT - IDAY) / DT
            TLAST = IDAY + DINT (X8) * DT + DT
            END IF
C                                       Spectral averaging
         IF ((DOSPEC) .AND. (IVSCNT.GT.0)) THEN
            CALL AVGCIF (VIS, NUMPLZ, NUMFRQ, BCHAN, ECHAN, BIF,
     *         EIF, CHNSEL, KNCS, VIS)
            END IF
C                                       Antenna numbers
         IF (ILOCB.GE.0) THEN
            JA1 = RPARM(ILOCB+1) / 256. + 0.1
            JA2 = RPARM(ILOCB+1) - JA1 * 256 + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       Find baseline
         DO 110 I = 1,NUMBAS
            IBASE = I
            IF ((JA1.EQ.ANTLAB(I,1) .AND. (JA2.EQ.ANTLAB(I,2))))
     *         GO TO 120
 110        CONTINUE
C                                       Not wanted
         GO TO 100
C
 120     GOTDAT = .TRUE.
C                                       Check if avg. or scan done
         IF ((INXRNO.GT.SCANUM) .OR. (IERR.LT.0) .OR.
     *      (CT.GT.TLAST)) GO TO 500
C                                       Time
         SUMTIM = SUMTIM + RPARM(ILOCT+1)
         SUMU = SUMU + RPARM(ILOCU+1) * UVSCAL
         SUMV = SUMV + RPARM(ILOCV+1) * UVSCAL
         SUMW = SUMW + RPARM(ILOCW+1) * UVSCAL
         IF (T1.GT.1.0E9) T1 = RPARM(ILOCT+1)
         CNTTIM = CNTTIM + 1
C                                       Source no.
         SUNUM = CURSOU
         IF (AMPVEC) GO TO 200
C                                       Vector average:
      INCLUDE 'INCS:ZVD.INC'
            DO 150 IVIS = 1,KVIS
               IF (VIS(3,IVIS).LE.0.0) GO TO 150
               COUNT(IBASE) = COUNT(IBASE) + 1
               WORK(IBASE,1) = WORK(IBASE,1) + VIS(1,IVIS)
               WORK(IBASE,2) = WORK(IBASE,2) + VIS(2,IVIS)
               WORK(IBASE,3) = WORK(IBASE,3) + VIS(1,IVIS) ** 2
               WORK(IBASE,4) = WORK(IBASE,4) + VIS(2,IVIS) ** 2
 150           CONTINUE
         GO TO 100
C                                       Ampscalar averaging
      INCLUDE 'INCS:ZVD.INC'
 200     DO 250 IVIS = 1,KVIS
            IF (VIS(3,IVIS).LE.0.0) GO TO 250
            AMP = SQRT (VIS(1,IVIS) * VIS(1,IVIS) +
     *         VIS(2,IVIS) * VIS(2,IVIS))
            VIS(1,IVIS) = VIS(1,IVIS) + 1.0E-20
            COUNT(IBASE) = COUNT(IBASE) + 1
            WORK(IBASE,1) = WORK(IBASE,1) + AMP
            IF (DTYPE.EQ.1) WORK(IBASE,2) = WORK(IBASE,2) +
     *         ATAN2 (VIS(2,IVIS), VIS(1,IVIS))
            WORK(IBASE,3) = WORK(IBASE,3) + AMP*AMP
 250        CONTINUE
            GO TO 100
C                                       Scan done
C                                       See if have any data.
 500  IF ((.NOT.GOTDAT) .AND. (IERR.EQ.0)) GO TO 10
      IF (.NOT.GOTDAT) GO TO 800
      IF (.NOT.AMPVEC) THEN
C                                       Vector averaging
         DO 550 I = 1,NUMBAS
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = SQRT (WORK(I,1)*WORK(I,1) +
     *            WORK(I,2)*WORK(I,2)) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = ATAN2 (WORK(I,2), WORK(I,1)+1.0E-20)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP1 = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               TEMP2 = (COUNT(I) * WORK(I,4) - (WORK(I,2)**2))
               TEMP = TEMP1 + TEMP2
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
 550           CONTINUE
      ELSE
C                                       Else Amp scalar ave
         DO 650 I = 1,NUMBAS
C                                       Amplitude
            IF ((DTYPE.EQ.0) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,1) / COUNT(I)
C                                       Phase
            IF ((DTYPE.EQ.1) .AND. (COUNT(I).GT.0))
     *         SCANV(I) = WORK(I,2) / COUNT(I)
C                                       RMS
            IF ((DTYPE.EQ.2) .AND. (COUNT(I).GT.2)) THEN
               TEMP = (COUNT(I) * WORK(I,3) - (WORK(I,1)**2))
               IF (TEMP.LT.0.0) TEMP = 0.0
               SCANV(I) = SQRT (TEMP) / COUNT(I)
               END IF
 650        CONTINUE
C                                       end if vector averaging
         END IF
C                                       Get source info
      IF (.NOT.NUSCAN) GO TO 800
      CALL GETSOU (SUNUM, IUDISK, IUCNO, CATUV, ISLUN, JERR)
      IF (JERR.EQ.11) THEN
C                                       Didn't find source
         WRITE (MSGTXT,1750) SUNUM
         CALL MSGWRT (8)
         JERR = 0
         END IF
      IF (JERR.LE.0) GO TO 800
         IERR = JERR
         WRITE (MSGTXT,1700) JERR
         GO TO 990
C                                       Time
 800  T1 = 0
      IF (CNTTIM.GT.0) THEN
         T1 = SUMTIM / CNTTIM
         AVTIM = T1
         AVU = SUMU / CNTTIM
         AVV = SUMV / CNTTIM
         AVW = SUMW / CNTTIM
         END IF
      CALL TODHMS (T1, TIME)
C                                       Set up new time boundary
      IDAY = CT
      X8 = (CT - IDAY) / DT
      TLAST = IDAY + DINT (X8) * DT + DT
C                                       If end of data, close UVGET
CCC      IF (IERR.LT.0) CALL UVGET ('CLOS', RPARM, VIS, JERR)
CCC      IF (JERR.NE.0) IERR = JERR
CCC      IF ((IERR.LT.0) .AND. (.NOT.GOTDAT)) IERR = -2
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1700 FORMAT ('BASAVG: ERROR',I3,' READING SOURCE TABLE')
 1750 FORMAT ('BASAVG: SOURCE ',I3,' NOT IN SU TABLE')
      END
