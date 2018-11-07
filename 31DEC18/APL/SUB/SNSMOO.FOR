      SUBROUTINE SNSMOO (XINTP, ALPHA, STAMP, STPH, SUB, NUMANT, TIMKOL,
     *   SUBKOL, ANTKOL, RF1KOL, MB1KOL, DE1KOL, RA1KOL, RE1KOL, IM1KOL,
     *   WT1KOL, RF2KOL, MB2KOL, DE2KOL, RA2KOL, RE2KOL, IM2KOL, WT2KOL,
     *   BUFFER, GNCNT, GNSUM, BUFF, NXT, WRK, IRET)
C-----------------------------------------------------------------------
C! Routine to smooth one IF in an SN table
C# Calibration table
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2000, 2007
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
C   Routine to smooth amplitudes and/or phases rates in an open SN
C   table.  All poln present are smoothed but only one IF.  The KOL
C   pointers are presumed to point at the desired IF.  An error is
C   returned if there are any non-zero delays, rates, or multi-band
C   delays.  If the reference antenna changes and phase is being
C   smoothed, an error is returned.
C   Input table must be in antenna-time order.
C   Inputs:
C      XINTP    C*4  Type of smoothing function: 'MWF', 'GAUS', else BOX
C      ALPHA    R    Alpha clip for MWF (0 -> box, 1 -> pure MWF)
C      STAMP    R    Amplitude smoothing time (days) < 0 => none, 0 =>
C                    fill in for blanked only.
C      STPH     R    Phase smoothing time (days)
C      SUB      I    Desired subarray
C      NUMANT   I    Number of antennas
C      TIMKOL   I    Time column pointer.
C      SUBKOL   I    Subarray column pointer
C      ANTKOL   I    Antenna column pointer
C      RF1KOL   I    Reference antenna 1
C      MB1KOL   I    Multiband delay 1 column pointer
C      DE1KOL   I    Singleband delay 1 column pointer
C      RA1KOL   I    Rate pol 1  column pointer
C      RE1KOL   I    Real 1  column pointer
C      IM1KOL   I    Imaginary 1  column pointer
C      WT1KOL   I    Weight 1 column pointer.
C      RF2KOL   I    Reference antenna 2
C      MB2KOL   I    Multiband delay 2 column pointer <1 => not present
C      DE2KOL   I    Singleband delay 2 column pointer <1 => not present
C      RA2KOL   I    Rate pol 2 pointer <1 => not present
C      RE2KOL   I    Real 2  column pointer <1 => not present
C      IM2KOL   I    Imaginary 2  column pointer
C      WT2KOL   I    Weight 2 column pointer <1 => not present
C      NXT      I    Number of times allowed in WRK
C   In/out:
C      BUFFER   I(512)   Table IO buffer
C      GNCNT    R    Count for gain normalization
C      GNSUM    R    Sum of gain modulii
C   Output:
C      BUFF     R    Wotk area >= NXT,m  (m=2 BOX, 3 GAUS, 4 MWF)
C      WRK      R    Work buffer (NXT,10)
C      IRET     I    Error code, 0=OK else failed., -1 ref changed
C                    -2 delay, rate, multiband non 0
C-----------------------------------------------------------------------
      CHARACTER XINTP*4
      INTEGER   SUB, NUMANT, TIMKOL, SUBKOL, ANTKOL, RF1KOL, MB1KOL,
     *   DE1KOL, RA1KOL, RE1KOL, IM1KOL, WT1KOL, RF2KOL,MB2KOL, DE2KOL,
     *   RA2KOL, RE2KOL, IM2KOL, WT2KOL, NXT, BUFFER(512), IRET
      REAL      ALPHA, STAMP, STPH, GNCNT, GNSUM, BUFF(NXT,*),
     *   WRK(NXT,10)
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, ITIME, REFA1, REFA2, N1GOOD, N2GOOD
      REAL      AMP, RSNRNO
      LOGICAL   NEED2
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
C                                       Buffers and file info
      INTEGER   RECI(XCLRSZ)
      REAL      RECR(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      EQUIVALENCE (RECI, RECR, RECD)
      EQUIVALENCE (ISNRNO, RSNRNO)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
C                                       Are there 2 polarizations
      NEED2 = RE2KOL.GT.0
      REFA1 = 0
      REFA2 = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
         N1GOOD = 0
         N2GOOD = 0
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECI, BUFFER, IRET)
            IF (IRET.LT.0) GO TO 100
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECI(ANTKOL).GT.ANT) GO TO 110
C                                       Want this record
            IF ((RECI(SUBKOL).EQ.SUB) .AND. (RECI(ANTKOL).EQ.ANT))
     *         THEN
C                                       Check for vlb etc
               IF ((RECR(MB1KOL).NE.FBLANK) .AND. (RECR(MB1KOL).NE.0.))
     *            GO TO 985
               IF ((RECR(DE1KOL).NE.FBLANK) .AND. (RECR(DE1KOL).NE.0.))
     *            GO TO 985
               IF ((RECR(RA1KOL).NE.FBLANK) .AND. (RECR(RA1KOL).NE.0.))
     *            GO TO 985
               IF (REFA1.LE.0) THEN
                  REFA1 = RECI(RF1KOL)
               ELSE
                  IF (RECI(RF1KOL).EQ.0) RECI(RF1KOL) = REFA1
                  END IF
               IF (REFA1.NE.RECI(RF1KOL)) GO TO 980
               IF (NEED2) THEN
                  IF ((RECR(MB2KOL).NE.FBLANK) .AND.
     *               (RECR(MB2KOL).NE.0.)) GO TO 985
                  IF ((RECR(DE2KOL).NE.FBLANK) .AND.
     *               (RECR(DE2KOL).NE.0.)) GO TO 985
                  IF ((RECR(RA2KOL).NE.FBLANK) .AND.
     *               (RECR(RA2KOL).NE.0.)) GO TO 985
                  IF (REFA2.LE.0) THEN
                     REFA2 = RECI(RF2KOL)
                  ELSE
                     IF (RECI(RF2KOL).EQ.0) RECI(RF2KOL) = REFA2
                     END IF
                  IF (REFA2.NE.RECI(RF2KOL)) GO TO 980
                  END IF
C                                       Put in buffer
               IF (NUMTIM.EQ.NXT) THEN
                  IRET = 1
                  WRITE (MSGTXT,1000) NXT
                  GO TO 990
                  END IF
               NUMTIM = NUMTIM + 1
               IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
               WRK(NUMTIM,9) = RECD(TIMKOL) - TIMOFF
               WRK(NUMTIM,10) = RSNRNO
               IF ((RECR(WT1KOL).GT.0.) .AND. (RECR(WT1KOL).NE.FBLANK))
     *            THEN
                  WRK(NUMTIM,3) = SQRT (RECR(RE1KOL)**2 +
     *               RECR(IM1KOL)**2)
               ELSE
                  WRK(NUMTIM,3) = 0.0
                  END IF
               IF (WRK(NUMTIM,3).GT.0.0) THEN
                  WRK(NUMTIM,1) = RECR(RE1KOL) / WRK(NUMTIM,3)
                  WRK(NUMTIM,2) = RECR(IM1KOL) / WRK(NUMTIM,3)
                  WRK(NUMTIM,4) = RECR(WT1KOL)
                  N1GOOD = N1GOOD + 1
               ELSE
                  WRK(NUMTIM,1) = 0.0
                  WRK(NUMTIM,2) = 0.0
                  WRK(NUMTIM,4) = 0.0
                  END IF
               IF (NEED2) THEN
                  IF ((RECR(WT2KOL).GT.0.) .AND.
     *               (RECR(WT2KOL).NE.FBLANK)) THEN
                     WRK(NUMTIM,7) = SQRT (RECR(RE2KOL)**2 +
     *                  RECR(IM2KOL)**2)
                  ELSE
                     WRK(NUMTIM,7) = 0.0
                     END IF
                  IF (WRK(NUMTIM,7).GT.0.0) THEN
                     WRK(NUMTIM,5) = RECR(RE2KOL) / WRK(NUMTIM,7)
                     WRK(NUMTIM,6) = RECR(IM2KOL) / WRK(NUMTIM,7)
                     WRK(NUMTIM,8) = RECR(WT2KOL)
                     N2GOOD = N2GOOD + 1
                  ELSE
                     WRK(NUMTIM,5) = 0.0
                     WRK(NUMTIM,6) = 0.0
                     WRK(NUMTIM,8) = 0.0
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
C                                       Smooth as requested
         IF (N1GOOD.GT.0) THEN
            IF (XINTP.EQ.'GAUS') THEN
               CALL SMGAUS (STAMP, WRK(1,9), WRK(1,3), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,3))
               CALL SMGAUS (STPH, WRK(1,9), WRK(1,1), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,1))
               CALL SMGAUS (STPH, WRK(1,9), WRK(1,2), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,4))
            ELSE IF (XINTP.EQ.'MWF') THEN
               CALL SMMWF (STAMP, ALPHA, WRK(1,9), WRK(1,3), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,3))
               CALL SMMWF (STPH, ALPHA, WRK(1,9), WRK(1,1), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,1))
               CALL SMMWF (STPH, ALPHA, WRK(1,9), WRK(1,2), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,4))
            ELSE
               CALL SMBOX (STAMP, WRK(1,9), WRK(1,3), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,3))
               CALL SMBOX (STPH, WRK(1,9), WRK(1,1), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,1))
               CALL SMBOX (STPH, WRK(1,9), WRK(1,2), WRK(1,4),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,4))
               END IF
            END IF
         IF (N2GOOD.GT.0) THEN
            IF (XINTP.EQ.'GAUS') THEN
               CALL SMGAUS (STAMP, WRK(1,9), WRK(1,7), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,7))
               CALL SMGAUS (STPH, WRK(1,9), WRK(1,5), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,5))
               CALL SMGAUS (STPH, WRK(1,9), WRK(1,6), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,6))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,8))
            ELSE IF (XINTP.EQ.'MWF') THEN
               CALL SMMWF (STAMP, ALPHA, WRK(1,9), WRK(1,7), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,7))
               CALL SMMWF (STPH, ALPHA, WRK(1,9), WRK(1,5), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,5))
               CALL SMMWF (STPH, ALPHA, WRK(1,9), WRK(1,6), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2), BUFF(1,3), BUFF(1,4))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,6))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,8))
            ELSE
               CALL SMBOX (STAMP, WRK(1,9), WRK(1,7), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,7))
               CALL SMBOX (STPH, WRK(1,9), WRK(1,5), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,5))
               CALL SMBOX (STPH, WRK(1,9), WRK(1,6), WRK(1,8),
     *            NUMTIM, BUFF(1,1), BUFF(1,2))
               CALL RCOPY (NUMTIM, BUFF(1,1), WRK(1,6))
               CALL RCOPY (NUMTIM, BUFF(1,2), WRK(1,8))
               END IF
            END IF
C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            RSNRNO = WRK(ITIME,10)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECI, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
C                                       Update
            IF (WRK(ITIME,4).GT.0.0) THEN
               AMP = SQRT (WRK(ITIME,1)**2 + WRK(ITIME,2)**2)
               IF (AMP.LE.0.0) AMP = 1.0
               RECR(RE1KOL) = WRK(ITIME,1) * WRK(ITIME,3) / AMP
               RECR(IM1KOL) = WRK(ITIME,2) * WRK(ITIME,3) / AMP
               RECR(WT1KOL) = WRK(ITIME,4)
               GNCNT = GNCNT + 1.0
               GNSUM = GNSUM + WRK(ITIME,3)
               IF (RECI(RF1KOL).EQ.0) RECI(RF1KOL) = REFA1
               END IF
            IF (NEED2) THEN
               IF (WRK(ITIME,8).GT.0.0) THEN
                  AMP = SQRT (WRK(ITIME,5)**2 + WRK(ITIME,6)**2)
                  IF (AMP.LE.0.0) AMP = 1.0
                  RECR(RE2KOL) = WRK(ITIME,5) * WRK(ITIME,7) / AMP
                  RECR(IM2KOL) = WRK(ITIME,6) * WRK(ITIME,7) / AMP
                  RECR(WT2KOL) = WRK(ITIME,8)
                  GNCNT = GNCNT + 1.0
                  GNSUM = GNSUM + WRK(ITIME,7)
                  IF (RECI(RF2KOL).EQ.0) RECI(RF2KOL) = REFA2
                  END IF
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECI, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200        CONTINUE
C                                       First SN number of next antenna
         FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      GO TO 990
 980  MSGTXT = 'SMSMOO: REFERENCE ANTENNA VARIES, CANNOT SMOOTH'
      GO TO 990
 985  MSGTXT = 'SMSMOO: NON-ZERO RATES OR DELAYS, CANNOT SMOOTH'
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNSMOO: MORE THAN',I9,' TIMES ENCOUNTERED, QUITTING')
 1900 FORMAT ('SNSMOO: TABIO ERROR',I3,' SMOOTHING AMP AND/OR PHASES')
      END
