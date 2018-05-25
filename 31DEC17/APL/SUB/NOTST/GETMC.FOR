      SUBROUTINE GETMC (MCBUFF, MCCOLS, MCNUMV, DELTAT, CLTIME, CLSRC,
     *   CLSTA, CLARR, CLFQI, CLOCK, DCLOCK, ATMOS, DATMOS, NUMREC,
     *   IRET)
C-----------------------------------------------------------------------
C! Extracts clock offsets and atmospheric delays from MC table
C# EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1999, 2008, 2011, 2014
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
C   Look up the clock offsets and atmospheric group delay corresponding
C   to a CL table record in a time-sorted model components table (MC).
C   The MC table must have been opened.
C   Inputs:
C      MCCOLS   I(21)    Column indices (from TABINI)
C      MCNUMV   I(21)    Column dimensions (from TABINI)
C      DELTAT   R        DELTAT keyword value from MCINI call
C      CLTIME   D        CL record time
C      CLSRC    I        CL record source number
C      CLSTA    I        CL record station number
C      CLARR    I        CL record subarry number
C      CLFQI    I        CL record FQ ID number
C
C   Input/output
C      MCBUFF   I(512)   I/O control block (from TABINI)
C      NUMREC   I        Set to zero to force a re-hash of file -
C                           leave alone otherwise
C
C   Output:
C      CLOCK    R(2)      Clock offset for each polarization in sec
C      DCLOCK   R(2)      Rates of change in clock offsets in sec/sec
C      ATMOS    R         Atmospheric group delay in sec
C      DATMOS   R         Rate of change of atmospheric group delay
C                            in sec/sec
C      IRET     I         Return code: 0 indicates lookup succeeded
C                            -1 => did not find within +- 1 minute
C                            anything else indicates unrecoverable error
C-----------------------------------------------------------------------
      INTEGER   MCBUFF(512), MCCOLS(21), MCNUMV(21), CLSRC, CLSTA,
     *   CLARR, CLFQI, NUMREC, IRET
      REAL      DELTAT, CLOCK(2), DCLOCK(2), ATMOS, DATMOS
      DOUBLE PRECISION CLTIME
C                                       Local variables:
C     DUALP     L     Is data present for both polarizations?
C
C     SROW      I     Starting row associated with CLTIME
C     EROW      I     Ending row associated with CLTIME
C     CURTIM    D     Actual time for rows SROW-EROW
C     TRANGE    R     Time range for MC records (days)
C     DTIME     R     Difference between CL and IM record times
C     VALID     L     Valid MC record found?
C     RECR      R(*)  Real-valued record
C     RECD      D(*)  Double-precison record
C     RECI      I(*)  Integer-values record
C     WARNLO    L     Has the user been warned about time before
C                      the start of the table?
C     WARNHI    L     Has the user been warned about times off the
C                      end of the table?
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   XMCRSZ, HASHSZ
      PARAMETER (XMCRSZ = 8 + 2 * 9 + 4 * MAXIF)
      PARAMETER (HASHSZ = 1024)
      LOGICAL   DUALP, VALID, WARNLO, WARNHI
      INTEGER   SROW, EROW, ROW, RECI(XMCRSZ), TROWS(HASHSZ), NTIMES, I,
     *  IB, IE, IANT, ISRC, IFQI, ISUB, MROW
      REAL      TRANGE, DTIME, RECR(XMCRSZ), TTIMES(HASHSZ), MTIME
      DOUBLE PRECISION CURTIM, RECD(XMCRSZ/2)
      EQUIVALENCE (RECD, RECI, RECR)
      SAVE      DUALP, SROW, EROW, CURTIM, TROWS, TTIMES, NTIMES
C
      INCLUDE 'INCS:DMSG.INC'
      DATA WARNLO /.FALSE./
      DATA WARNHI /.FALSE./
C-----------------------------------------------------------------------
      TRANGE = DELTAT
      IF (DELTAT.LE.0.0) TRANGE = 5.1 / (24.0 * 60.0)
C                                       re-init if change or first call
      IF (NUMREC.NE.MCBUFF(5)) THEN
         NUMREC = MCBUFF(5)
         DUALP  = MCBUFF(10).GT.15
         SROW = 0
         EROW = 0
         CURTIM = -999.0
         CALL FILL (HASHSZ, NUMREC, TROWS)
         CALL RFILL (HASHSZ, 9999.0, TTIMES)
         NTIMES = MIN (HASHSZ, NUMREC)
         DO 20 I = 1,NTIMES
            IF (NTIMES.LE.1) THEN
               ROW = 1
            ELSE
               ROW = (NUMREC-1.0) * (I-1.0) / (NTIMES - 1.0) + 1.5
               END IF
            TROWS(I) = ROW
            CALL TABIO ('READ', 0, ROW, RECR, MCBUFF, IRET)
            IF (IRET.GT.0) GO TO 990
            TTIMES(I) = RECD(MCCOLS(1))
 20         CONTINUE
         END IF
C                                       Fill-in default values:
      CLOCK(1) = 0.0
      CLOCK(2) = 0.0
      DCLOCK(1) = 0.0
      DCLOCK(2) = 0.0
      ATMOS = 0.0
      DATMOS = 0.0
      IRET = -1
C                                       check total range
      IF (CLTIME.LT.TTIMES(1)-TRANGE) THEN
         IF (.NOT.WARNLO) THEN
            WARNLO = .TRUE.
            MSGTXT = 'WARNING: times encountered before start' //
     *         ' of MC table'
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
      IF (CLTIME.GT.TTIMES(NTIMES)+TRANGE) THEN
         IF (.NOT.WARNHI) THEN
            WARNHI = .TRUE.
            MSGTXT = 'WARNING: times encountered after end' //
     *         ' of MC table'
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
C                                       search hash list
      IB = 1
      IE = NUMREC
      DO 30 I = 1,NTIMES
         IF (TTIMES(I).LT.CLTIME-TRANGE) IB = TROWS(I)
         IF (TTIMES(NTIMES+1-I).GT.CLTIME+TRANGE)
     *      IE = TROWS(NTIMES-I+1)
 30      CONTINUE
C                                       Search records SROW to EROW for
C                                       CLSRC and CLSTA
      MROW = 0
      MTIME = 1.E9
      DO 70 ROW = IB,IE
         CALL TABIO ('READ', 0, ROW, RECR, MCBUFF, IRET)
         IF (IRET.GT.0) GO TO 990
         IF (IRET.EQ.0) THEN
            IANT = RECI(MCCOLS(3))
            ISRC = RECI(MCCOLS(2))
            ISUB = RECI(MCCOLS(4))
            IFQI = RECI(MCCOLS(5))
            VALID = ((IANT.LE.0) .OR. (CLSTA.LE.0) .OR. (CLSTA.EQ.IANT))
            VALID = ((ISRC.LE.0) .OR. (CLSRC.LE.0) .OR. (CLSRC.EQ.ISRC))
     *         .AND. VALID
            VALID = ((ISUB.LE.0) .OR. (CLARR.LE.0) .OR. (CLARR.EQ.ISUB))
     *         .AND. VALID
            VALID = ((IFQI.LE.0) .OR. (CLFQI.LE.0) .OR. (CLFQI.EQ.IFQI))
     *         .AND. VALID
            IF (VALID) THEN
               DTIME  = CLTIME - RECD(MCCOLS(1))
               IF (ABS(DTIME).LT.MTIME) THEN
                  MTIME = ABS(DTIME)
                  MROW = ROW
                  END IF
               END IF
            END IF
 70      CONTINUE
      IF (MROW.GT.0) THEN
         ROW = MROW
         CALL TABIO ('READ', 0, ROW, RECR, MCBUFF, IRET)
         IF (IRET.GT.0) GO TO 990
         IF (IRET.EQ.0) THEN
            DTIME     = CLTIME - RECD(MCCOLS(1))
            DATMOS    = RECD(MCCOLS(7))
            ATMOS     = RECD(MCCOLS(6)) + DTIME * DATMOS * 86400
            DCLOCK(1) = RECD(MCCOLS(11))
            CLOCK(1)  = RECD(MCCOLS(10)) + DTIME * DCLOCK(1) * 86400
            IF (DUALP) THEN
               DCLOCK(2) = RECD(MCCOLS(17))
               CLOCK(2)  = RECD(MCCOLS(16)) + DTIME * DCLOCK(2) *
     *            86400
               END IF
            END IF
      ELSE
         IRET = -1
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, ROW
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GETMC: ERROR ',I4,' READING MC TABLE ROW',I10)
      END
