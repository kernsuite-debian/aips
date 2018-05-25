      SUBROUTINE DATBND (TIME, IA1, IA2, VIS, IERR)
C-----------------------------------------------------------------------
C! Applies the bandpass correction to data.
C# Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2001, 2003, 2005-2006, 2010-2011, 2013-2015
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
C   Routine which applies the bandpass correction.
C   Inputs:
C      TIME     R        Time of visibility data (in days)
C      IA1      I        Antenna number 1
C      IA2      I        Antenna number 2
C   Inputs from common:
C      BPBUFF   R(*)     Large array containing bandpass spectra for
C                        all antennas
C      ANTPNT   LI(2,2)  Pointer giving the start address of the
C                        specified antennas BP spectra within BPBUFF
C      DOBAND   I        Method of BP application
C   In/Outputs:
C      VIS      R(*)     Array of visibility data (corrected on out)
C   Output:
C      IERR     I        If > 0, error returned from BPGET
C   Output to common:
C      CNTREC  I(2,3)  Record counts:
C                        (1&2,1) Previously flagged (partly, fully)
C                        (1&2,2) Flagged due to gains (part, full)
C                        (1&2,3) Good selected (part, full)
C   NOTE: This routine applies the bandpass correction for formulae:
C      (1) Cross-power:   Scorr   =  (1/Sant_1) * (1/Sant_2) * Sobs
C      (2) Total-power:   Scorr   =  (Son / Soff) - 1.0
C-----------------------------------------------------------------------
      REAL      TIME, VIS(*)
      INTEGER   IA1, IA2, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   INCPX, IFRQ, IIF, IOFF, DINDX, CHOFF, NCOR, KLOCS, JERR,
     *   TCOR0, IPOL, JPOL1(4), JPOL2(4), LRECS, LUNSRC, IFLAG(2),
     *   JFLAG(2), IROUND, KLOCF, ITMP, JTMP, NMAX, LBCH, LECH, CH1,
     *   CH2, NCHS(4)
      LONGINT   I1OFF, I2OFF, L1OFF, L2OFF, I1D, I2D, L1D, L2D
      REAL      GR1, GI1, TVR, TVI, AMP12, AMP22, REAL1, IMAG1, REAL2,
     *   IMAG2, RTMP, BPARR1(6), BPARR2(6), AMPS(4)
      HOLLERITH CATH(256)
      LOGICAL   FLAGD1, FLAGDA, IQUV, CORRP, T, F, WAUTO, FLAGD0, FAILED
      DOUBLE PRECISION CATD(128), SHIFT1(MAXIF), SHIFT2(MAXIF), RATE,
     *   TFRQ, DPOLYN(MAXCHA)
      SAVE DPOLYN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATH, CATD, CATUV)
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Check sizes
      IF ((NIFBP.GT.MAXIF) .OR. (NCHNBP.GT.MAXCHA) .OR.
     *   (NPOLBP*NIFBP*NCHNBP.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'DATBND:VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
      IF (BCHANS.LE.0) BCHANS = BCHAN
      IF (ECHANS.LE.0) ECHANS = ECHAN
C                                       Average amplitude range
      ITMP = CATUV(KINAX+KLOCFY)
      IF (ISVLA) THEN
         CH1 = (ITMP+1)/8 + 1
         CH2 = ITMP - ((ITMP+1)/8)
      ELSE
         CH1 = 1
         CH2 = ITMP
         END IF
      CALL FILL (4, 0, NCHS)
      CALL RFILL (4, 0.0, AMPS)
      IF (DOWTCL) THEN
         LBCH = MIN (CH1, BCHANS)
         LECH = MAX (CH2, ECHANS)
      ELSE
         LBCH = BCHANS
         LECH = ECHANS
         END IF
C                                       Determine shift needed from BP
C                                       table
      CALL DFILL (MAXIF, 0.0D0, SHIFT1)
      CALL DFILL (MAXIF, 0.0D0, SHIFT2)
C
      IF (ISVLBA) THEN
         CALL AXEFND (8, 'FREQ    ', CATUV(KIDIM), CATH(KHCTP), KLOCF,
     *      JERR)
         IF ((RAEPO.EQ.0.D0) .AND. (DECEPO.EQ.0.D0)) THEN
            LUNSRC = 49
            CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, LUNSRC, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR
               GO TO 990
               END IF
            END IF
         DO 20 IIF = 1,CNNIF
            TFRQ = CATD(KDCRV+KLOCF) + CFOFF(IIF) + FREQO(IIF)
            CALL DETRAT (TIME, RAEPO, DECEPO, ANTX(IA1), ANTY(IA1),
     *         ANTZ(IA1), TFRQ, RATE)
            SHIFT1(IIF) = RATE / CFINC(IIF)
            IF (IA2.NE.IA1) THEN
               CALL DETRAT (TIME, RAEPO, DECEPO, ANTX(IA2), ANTY(IA2),
     *            ANTZ(IA2), TFRQ, RATE)
               SHIFT2(IIF) = RATE / CFINC(IIF)
               END IF
 20         CONTINUE
         END IF
C                                       Get the bandpass spectra
      CALL BPGET (TIME, IA1, IA2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      FAILED = IERR.LT.0
      IERR = 0
C                                       Check STOKES parms
      CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATH(KHCTP), KLOCS,
     *   JERR)
      NCOR = CATUV(KINAX+KLOCS)
      IF (CATD(KDCRV+KLOCS).GT.0.0D0) TCOR0 = CATD(KDCRV+KLOCS) + 0.5D0
      IF (CATD(KDCRV+KLOCS).LT.0.0D0) TCOR0 = CATD(KDCRV+KLOCS) - 0.5D0
      IQUV = TCOR0.GE.1
      CORRP = (TCOR0.LE.-1) .AND. (TCOR0.GT.-9)
C                                       Set visibility increment
      INCPX = CATUV(KINAX)
C                                       Compressed data expanded
      IF (INCPX.EQ.1) INCPX = 3
C                                       Set up base pointers
      I1OFF = ANTPNT(1,1)
      I2OFF = ANTPNT(2,1)
      L1OFF = ANTPNT(1,1) + PVLBUF - PBPBUF
      L2OFF = ANTPNT(2,1) + PVLBUF - PBPBUF
C                                       polarization pointers
      DO 30 IPOL = 1,NCOR
         IF (CORRP) THEN
C                                       RR or LL
            IF (IPOL.LE.2) THEN
               JPOL1(IPOL) = IPOL
               JPOL2(IPOL) = IPOL
C                                       LL only case has been removed
C              IF ((NCOR.EQ.1) .AND. (TCOR0.EQ.-2)) THEN
C                 JPOL1(IPOL) = IPOL + 1
C                 JPOL2(IPOL) = IPOL + 1
C                 END IF
C                                       RL
            ELSE IF (IPOL.EQ.3) THEN
               JPOL1(IPOL) = 1
               JPOL2(IPOL) = 2
C                                       LR
            ELSE IF (IPOL.EQ.4) THEN
               JPOL1(IPOL) = 2
               JPOL2(IPOL) = 1
               END IF
C                                       For IQUV always use Stokes I
C                                       bandpass
         ELSE IF (IQUV) THEN
            JPOL1(IPOL) = 1
            JPOL2(IPOL) = 1
            END IF
 30      CONTINUE
C                                       Flag on total failure
      IF (FAILED) THEN
         FLAGD1 = T
         FLAGDA = T
         DO 45 IIF = BIF,EIF
            IOFF = (IIF-1) * KNCIF
            DO 40 IFRQ = BCHANS,ECHANS
               DO 35 IPOL = 1,NCOR
C                                       Index for weight
                  DINDX = ((IOFF + (IPOL-1) * KNCS) +
     *               (IFRQ-1) * KNCF) * INCPX + 3
                  VIS(DINDX) = -1.0
 35               CONTINUE
 40            CONTINUE
 45         CONTINUE
C                                       Shift the bandpasses to the
C                                       appropriate value. First set
C                                       flagged channels.
      ELSE
         FLAGD1 = F
         FLAGDA = T
         IFLAG(1) = 0
         IFLAG(2) = 0
         JFLAG(1) = NCHNBP + 1
         JFLAG(2) = NCHNBP + 1
         IF (ISVLBA) THEN
            IFLAG(1) = IROUND (AVDELI(IA1,1)) + 1
            JFLAG(1) = NCHNBP - IFLAG(1) + 1
            IF (ABS (SHIFT1(1)).GT.0.0) THEN
               RTMP = ABS (SHIFT1(1))
               ITMP = 1 + IROUND (RTMP)
               IFLAG(1) = MAX(IFLAG(1), ITMP)
               JTMP = NCHNBP - IFLAG(1) + 1
               JFLAG(1) = MIN(JFLAG(1), JTMP)
               END IF
            IF (IA1.NE.IA2) THEN
               IFLAG(2) = IROUND(AVDELI(IA2,1)) + 1
               JFLAG(2) = NCHNBP - IFLAG(2) + 1
               IF (ABS (SHIFT2(1)).GT.0.0) THEN
                  RTMP = ABS (SHIFT2(1))
                  ITMP = 1 + IROUND (RTMP)
                  IFLAG(2) = MAX(IFLAG(2), ITMP)
                  JTMP = NCHNBP - IFLAG(2) + 1
                  JFLAG(2) = MIN(JFLAG(2), JTMP)
                  END IF
               END IF
            END IF
C                                       Case BP entry type of:
C                                       1: Polynomial BP
         IF (WPOLY) THEN
            WAUTO = (IA1.EQ.IA2)
            DO 60 IPOL = 1,NPOLBP
               DO 50 IIF = 1,NIFBP
                  I1D = I1OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  I2D = I2OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L1D = L1OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L2D = L2OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  CALL BPCOEF (LTYPBP, BPBUF(I1D), BPBUF(I1D+1),
     *               KSNCF, KSNCF, NCHNBP, FBLANK, SHIFT1(IIF),
     *               VLBUF(L1D), VLBUF(L1D+1), KSNCF, KSNCF,
     *               1, NCHNBP, 1.0, FLOAT(NCHNBP), 0, WAUTO,
     *               DPOLYN, MAXCHA, NMAX, .FALSE., IERR)
                  IF (IERR.NE.0) GO TO 999
                  IF (.NOT.WAUTO) CALL BPCOEF (LTYPBP, BPBUF(I2D),
     *               BPBUF(I2D+1), KSNCF, KSNCF, NCHNBP, FBLANK,
     *               SHIFT2(IIF), VLBUF(L2D), VLBUF(L2D+1), KSNCF,
     *               KSNCF, 1, NCHNBP, 1.0, FLOAT(NCHNBP), 0, WAUTO,
     *               DPOLYN, MAXCHA, NMAX, .FALSE., IERR)
                  IF (IERR.NE.0) GO TO 999
 50               CONTINUE
 60            CONTINUE
C                                       2: Standard BP entry:
         ELSE IF (ISVLBA) THEN
            LRECS = NIFBP * NPOLBP * NCHNBP * 2
            IF ((ABS(SHIFT1(1) - CURSHF(IA1))) .GT. 0.1) THEN
               CALL RCOPY (LRECS, BPBUF(I1OFF), VLBUF(L1OFF))
               CURSHF(IA1) = SHIFT1(1)
               CALL BPSHFT (VLBUF(L1OFF), NPOLBP, NIFBP, NCHNBP,
     *            KSNCS, KSNCIF, KSNCF, SHIFT1, NUMSHF)
               END IF
            IF (IA1.NE.IA2) THEN
               IF ((ABS(SHIFT2(1) - CURSHF(IA2))) .GT. 0.1) THEN
                  CALL RCOPY (LRECS, BPBUF(I2OFF), VLBUF(L2OFF))
                  CURSHF(IA2) = SHIFT2(1)
                  CALL BPSHFT (VLBUF(L2OFF), NPOLBP, NIFBP, NCHNBP,
     *               KSNCS, KSNCIF, KSNCF, SHIFT2, NUMSHF)
                  END IF
               END IF
            END IF
C                                       Endcase (BP_entry type)
C
C                                       Additional offset to cope
C                                       with the possibility that
C                                       BCHAN and BCHNBP differ
         CHOFF = 1 - BCHNBP
C                                       Check bandpass and data match
         IF (BCHNBP.GT.BCHAN) THEN
            WRITE (MSGTXT,1010) BCHAN
            CALL MSGWRT (7)
            WRITE (MSGTXT,1020) BCHNBP
            CALL MSGWRT (7)
            WRITE (MSGTXT,1030)
            IERR = 5
            GO TO 990
            END IF
         DO 150 IIF = BIF,EIF
            IOFF = (IIF-1) * KNCIF
            DO 120 IFRQ = LBCH,LECH
C                                       Generate the correction arrays
               DO 100 IPOL = 1,NPOLBP
                  I1D = I1OFF + (IFRQ-1+CHOFF) * KSNCF +
     *               (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L1D = I1D - I1OFF + L1OFF
                  DINDX = IPOL * 3 - 2
                  BPARR1(DINDX+2) = 1.0
                  BPARR2(DINDX+2) = 1.0
C                                       cross power
                  I2D = I2OFF + (IFRQ-1+CHOFF) * KSNCF +
     *               (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L2D = I2D - I2OFF + L2OFF
                  IF (ISVLBA .OR. WPOLY) THEN
                     AMP12 = (VLBUF(L1D)   * VLBUF(L1D) +
     *                  VLBUF(L1D+1) * VLBUF(L1D+1))
                     AMP22 = (VLBUF(L2D)   * VLBUF(L2D) +
     *                  VLBUF(L2D+1) * VLBUF(L2D+1))
                     IF (IFRQ.LE.IFLAG(1)) VLBUF(L1D) = FBLANK
                     IF (IFRQ.LE.IFLAG(2)) VLBUF(L2D) = FBLANK
                     IF (IFRQ.GE.JFLAG(1)) VLBUF(L1D) = FBLANK
                     IF (IFRQ.GE.JFLAG(2)) VLBUF(L2D) = FBLANK
                     FLAGD0 = F
                     IF ((AMP12.LE.0.0) .OR. (VLBUF(L1D).EQ.FBLANK)
     *                  .OR. (VLBUF(L1D+1).EQ.FBLANK)) THEN
                        BPARR1(DINDX+2) = -1.0
                        AMP12 = 1.0
                        FLAGD0 = T
                        END IF
                     IF ((AMP22.LE.0.0) .OR. (VLBUF(L2D).EQ.FBLANK)
     *                  .OR. (VLBUF(L2D+1).EQ.FBLANK)) THEN
                        BPARR2(DINDX+2) = -1.0
                        AMP22 = 1.0
                        FLAGD0 = T
                        END IF
                     IF (FLAGD0) THEN
                        FLAGD1 = T
                     ELSE
                        FLAGDA = F
                        END IF
                     BPARR1(DINDX) = VLBUF(L1D)   / AMP12
                     BPARR1(DINDX+1) = VLBUF(L1D+1) / AMP12
                     BPARR2(DINDX) = VLBUF(L2D)   / AMP22
                     BPARR2(DINDX+1) = VLBUF(L2D+1) / AMP22
                  ELSE
                     AMP12 = (BPBUF(I1D)   * BPBUF(I1D) +
     *                  BPBUF(I1D+1) * BPBUF(I1D+1))
                     AMP22 = (BPBUF(I2D)   * BPBUF(I2D) +
     *                  BPBUF(I2D+1) * BPBUF(I2D+1))
                     FLAGD0 = F
                     IF ((AMP12.LE.0.0) .OR. (BPBUF(I1D).EQ.FBLANK)
     *                  .OR. (BPBUF(I1D+1).EQ.FBLANK)) THEN
                        BPARR1(DINDX+2) = -1.0
                        AMP12 = 1.0
                        FLAGD0 = T
                        END IF
                     IF ((AMP22.LE.0.0) .OR. (BPBUF(I2D).EQ.FBLANK)
     *                  .OR. (BPBUF(I2D+1).EQ.FBLANK)) THEN
                        BPARR2(DINDX+2) = -1.0
                        AMP22 = 1.0
                        FLAGD0 = T
                        END IF
                     IF (FLAGD0) THEN
                        FLAGD1 = T
                     ELSE
                        FLAGDA = F
                        END IF
                     BPARR1(DINDX) = BPBUF(I1D)   / AMP12
                     BPARR1(DINDX+1) = BPBUF(I1D+1) / AMP12
                     BPARR2(DINDX) = BPBUF(I2D)   / AMP22
                     BPARR2(DINDX+1) = BPBUF(I2D+1) / AMP22
                     END IF
 100              CONTINUE
C                                       Apply the correction
               DO 110 IPOL = 1,NCOR
C                                       Index for visibility
                  DINDX = ((IOFF + (IPOL-1) * KNCS) +
     *               (IFRQ-1) * KNCF) * INCPX + 1
                  I1D = 3*JPOL1(IPOL) - 2
                  I2D = 3*JPOL2(IPOL) - 2
C                                       Do the correction
                  REAL1 = BPARR1(I1D)
                  IMAG1 = BPARR1(I1D+1)
                  REAL2 = BPARR2(I2D)
                  IMAG2 = BPARR2(I2D+1)
C                                       Cross-hands use conjugate of
C                                       second.
C                 IF ((CORRP .AND. (IPOL.GT.2)) .AND. (IA1.NE.IA2))
C     *              IMAG2 = -IMAG2
C
                  GR1 = (REAL1*REAL2 + IMAG1*IMAG2)
                  GI1 = (REAL2*IMAG1 - REAL1*IMAG2)
C
                  TVR = GR1 * VIS(DINDX) + GI1 * VIS(DINDX+1)
                  TVI = GR1 * VIS(DINDX+1) - GI1 * VIS(DINDX)
                  VIS(DINDX) = TVR
                  VIS(DINDX+1) = TVI
C                 IF (IA1.EQ.IA2) THEN
C                    IF ((CORRP .AND. (IPOL.LT.3)) .OR. IQUV)
C    *                  VIS(DINDX) = VIS(DINDX) - 1.0
C                    END IF
                  IF ((BPARR1(I1D+2).LE.0.0) .OR.
     *               (BPARR2(I2D+2).LE.0.0)) THEN
                     VIS(DINDX+2) = -1.0
                  ELSE IF (DOWTCL) THEN
                     IF ((IFRQ.GE.CH1) .AND. (IFRQ.LE.CH2)) THEN
                        AMPS(IPOL) = AMPS(IPOL) + SQRT (GR1*GR1+GI1*GI1)
                        NCHS(IPOL) = NCHS(IPOL) + 1
                        END IF
                     END IF
 110              CONTINUE
 120           CONTINUE
C                                       calibrate weights
            IF (DOWTCL) THEN
               DO 140 IPOL = 1,NCOR
                  IF ((NCHS(IPOL).GT.0) .AND. (AMPS(IPOL).GT.0.0)) THEN
                     RTMP = NCHS(IPOL) / AMPS(IPOL)
                     RTMP = RTMP * RTMP
                     IF (ABS(RTMP-1.0).GT.0.05) THEN
                        DO 130 IFRQ = BCHANS,ECHANS
                           DINDX = ((IOFF + (IPOL-1) * KNCS) +
     *                        (IFRQ-1) * KNCF) * INCPX + 3
                           IF (VIS(DINDX).GT.0.0) VIS(DINDX) =
     *                        VIS(DINDX) * RTMP
 130                       CONTINUE
                        END IF
                     END IF
 140              CONTINUE
               END IF
 150        CONTINUE
         END IF
C                                       Update error counts
      IF (FLAGD1) THEN
         IF (FLAGDA) THEN
            CNTREC(2,2) = CNTREC(2,2) + 1
         ELSE
            CNTREC(1,2) = CNTREC(1,2) + 1
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATBND: ERROR ',I3,' RETURNED FROM BPGET')
 1010 FORMAT ('DATBND: START CHANNEL IN DATA = ',I3)
 1020 FORMAT ('DATBND: START CHANNEL IN BP TABLE = ',I3)
 1030 FORMAT ('DATBND: CANNOT DO BANDPASS CORRECTION - RESET BP TABLE')
 1040 FORMAT ('DATBND: ERROR ',I2,' DETERMINING SOURCE POSITION')
      END
