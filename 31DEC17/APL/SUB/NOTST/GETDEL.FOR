      SUBROUTINE GETDEL (CLTIME, CLSRC, CLSTA, CLARR, CLFQID,
     *   IMBUFF, GEODLY, DISP, DDISP, NUMREC, IRET)
C-----------------------------------------------------------------------
C! Moves delay polynomials from IM to CL tables with time shift
C# EXT-appl VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-2000, 2008, 2014
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
C   Look up the geometric delay polynomial for CLTIME, CLSRC, CLSTA, and
C   CLFQID in an IM table and return it as GEODLY with it's time origin
C   shifted to CLTIME.  The contents of GEODLY are suitable for use in
C   a CL table record defined by CLTIME, CLSRC, CLSTA, CLFQID.
C
C   If no IM table record can be found covering CLTIME an earlier one
C   will be used providing that CLTIME - IMTIME < 2 * IMTINT where
C   IMTIME is the reference time for the IM table record and IMTINT is
C   the time interval covered by the IM table record: a warning message
C   will be printed if CLTIME - IMTIME > 1.1.
C
C   If no IM table record covers CLTIME with the extended duration then
C   the polynomial will be set to all zeros and a warning message will
C   be printed.
C
C   The delay polynomial is assumed to have no more than 17 terms.
C
C   The IM table is assumed to be in order of ascending time and
C   calls to GETDEL should be made in ascending order of CLTIME.
C
C   Inputs:
C      CLTIME   D      CL entry time (days)
C      CLSRC    I      CL entry source number
C      CLSTA    I      CL entry antenna number
C      CLARR    I      CL entry array number (currently ignored)
C      CLFQID   I      CL entry frequency ID
C
C   Input/output:
C      NUMREC   I      Set to 0 to force re-hash of file -
c                        leave alone otherwise
C      IMBUFF   I(*)   I/O buffer and control block for open IM table
C
C   Output:
C      GEODLY   D(*)   Delay polynomial
C      DISP     R(2)   Dispersive delay (sec)
C      DDISP    R(2)   Rate of change of DISP (sec/sec)
C      IRET     I      Return status: 0 -> successful
C                                         other -> table I/O error
C-----------------------------------------------------------------------
      DOUBLE PRECISION CLTIME, GEODLY(*)
      INTEGER   CLSRC, CLSTA, CLARR, CLFQID, IMBUFF(*), NUMREC, IRET
      REAL      DISP(2), DDISP(2)
C
C                                       Saved variables
C     NUMREC       I      Number of records in the IM table.
C     IMTIME       D      Reference time of best candidate IM table
C                         record
C     IMTINT       R      Nominal duration of best candidate IM table
C                         record
C     SROW,EROW    I      Row number range around current time
C     TROWS        I(*)   Hash table - row numbers
C     TTIMES       R(*)   Hash table - times
C     TINTS        R(*)   Has table increments
C
      INTEGER   HASHSZ
      PARAMETER (HASHSZ = 1024)
      INTEGER   TDHMS(4), NTIMES, TROWS(HASHSZ), IB, IE, SROW, EROW, I,
     *   ROW, MROW
      REAL      IMTINT, TTIMES(HASHSZ), TINTS(HASHSZ), TMPINT, TDI(2),
     *   TDD(2), MTIME
      DOUBLE PRECISION IMTIME, IMPOLY(17), DELTIM, TMPTIM
      LOGICAL   WARNLO, WARNHI, VALID
C
      SAVE      SROW, EROW, NTIMES, TROWS, TTIMES, TINTS, WARNLO,
     *   WARNHI, IMTINT, IMTIME
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA WARNLO, WARNHI /2 * .FALSE./
C-----------------------------------------------------------------------
C                                       NUMREC change => reset hash
C                                       table since file has changed
      IF (IMBUFF(5).NE.NUMREC) THEN
         NUMREC = IMBUFF(5)
         CALL FILL (HASHSZ, NUMREC, TROWS)
         CALL RFILL (HASHSZ, 9999.0, TTIMES)
         NTIMES = MIN (HASHSZ, NUMREC)
         IMTIME = -999.0D0
         IMTINT = 1.E-9
         DO 20 I = 1,NTIMES
            IF (NTIMES.LE.1) THEN
               ROW = 1
            ELSE
               ROW = (NUMREC-1.0) * (I-1.0) / (NTIMES - 1.0) + 1.5
               END IF
            TROWS(I) = ROW
            IIMRNO = ROW
            CALL IMTAB ('READ', IMBUFF, NOBAND, IRET)
            IF (IRET.GT.0) GO TO 990
            TTIMES(I) = TIME
            TINTS(I) = TINT * 2.0
 20         CONTINUE
         END IF
C                                       Fill-in default values:
      CALL DFILL (NPOLY, 0.0D0, GEODLY)
      DISP(1) = 0.0
      DISP(2) = 0.0
      DDISP(1) = 0.0
      DDISP(2) = 0.0
C                                       check total range
      IF (CLTIME.LT.TTIMES(1)) THEN
         IF (.NOT.WARNLO) THEN
            WARNLO = .TRUE.
            MSGTXT = 'WARNING: times encountered before start' //
     *         ' of IM table'
            GO TO 900
         ELSE
            GO TO 910
            END IF
         END IF
      IF (CLTIME.GT.TTIMES(NTIMES)+TINTS(NTIMES)) THEN
         IF (.NOT.WARNHI) THEN
            WARNHI = .TRUE.
            MSGTXT = 'WARNING: times encountered after end' //
     *         ' of MC table'
            GO TO 900
         ELSE
            GO TO 910
            END IF
         END IF
C
C        To minimize the amount of table reading we need to set bounds
C        on the rows to be searched so that the sections of the table
C        that contain no candidate matches are not read. We start by
C        setting bounds IB and IE on the has table so that for all
C        i such that 1 <= i < IB, TTIMES(i) + TINTS(i) < CLTIME and
C        for all i such that IE < i <= NTIMES, TTIMES(i) < CLTIME
C        since a record with hash table index j can only be valid if
C        TTIMES(j) <= CLTIME <= TTIMES(j) + TINTS(j)
C
      IB = 1
 30   IF ((IB.LT.NTIMES) .AND. (TTIMES(IB)+TINTS(IB).LT.CLTIME)) THEN
         IB = IB + 1
         GO TO 30
         END IF
      IE = NTIMES
 40   IF ((IE.GT.IB) .AND. (TTIMES(IE).GT.CLTIME)) THEN
         IE = IE - 1
         GO TO 40
         END IF
C
C        If there are more IM records than hash table entries then
C        there may be potentialy valid records with
C        TROWS(IB-1) < row < TROWS(IB) or TROWS(IE) < row < TROWS(IE+1)
C        so adjust IB and IE to make sure these rows are not excluded:
C
      IF (IB.GT.1) IB = IB - 1
      IF (IE.LT.NTIMES) IE = IE + 1
C
C        Now translate the hash indices to row numbers:
C
      SROW = TROWS(IB)
      EROW = TROWS(IE)
C                                       Search records SROW to EROW for
C                                       CLSRC and CLSTA
C                                       and closest time
      TMPTIM = -999.9D0
      TMPINT = 0.0
      MROW = 0
      MTIME = 1.E9
      DO 70 ROW = SROW,EROW
         IIMRNO = ROW
         CALL IMTAB ('READ', IMBUFF, NOBAND, IRET)
         IF (IRET.GT.0) GO TO 990
         IF (IRET.EQ.0) THEN
            VALID = (CLTIME-TIME).LE.2.0*TINT
            VALID = VALID .AND. ((CLTIME-TIME).GT.-TINT/200.0)
            VALID = ((NOSTA.LE.0) .OR. (CLSTA.LE.0) .OR.
     *         (CLSTA.EQ.NOSTA)) .AND. VALID
            VALID = ((ISRC.LE.0) .OR. (CLSRC.LE.0) .OR. (CLSRC.EQ.ISRC))
     *         .AND. VALID
            VALID = ((IARRAY.LE.0) .OR. (CLARR.LE.0) .OR.
     *         (CLARR.EQ.IARRAY)) .AND. VALID
            VALID = ((IFQID.LE.0) .OR. (CLFQID.LE.0) .OR.
     *         (CLFQID.EQ.IFQID)) .AND. VALID
            IF (VALID) THEN
               IF (ABS(CLTIME-TIME).LT.MTIME) THEN
                  MROW = ROW
                  MTIME = ABS(CLTIME-TIME)
                  END IF
               END IF
            END IF
 70      CONTINUE
      IF (MROW.GT.0) THEN
         IIMRNO = MROW
         CALL IMTAB ('READ', IMBUFF, NOBAND, IRET)
         IF (IRET.GT.0) GO TO 990
         IF (IRET.EQ.0) THEN
            TMPTIM = TIME
            TMPINT = TINT
            CALL DPCOPY (NPOLY, GDELA1, IMPOLY)
            TDI(1) = DISP1
            TDD(1) = DDISP1
            TDI(2) = DISP2
            TDD(2) = DDISP2
            IF ((CLTIME-TIME.LT.TINT) .AND. (CLTIME-TIME.GE.0.0))
     *         GO TO 80
            END IF
         END IF
      IF (TMPTIM.LT.0.0D0) GO TO 910
C                                       found one
 80   IMTIME = TMPTIM
      IMTINT = TMPINT
C                                       IMTIME, IMTINT, and IMPOLY now
C                                       contain data from the latest
C                                       applicable IM table record.
      IF (ABS(CLTIME-IMTIME).GT.1.10*IMTINT) THEN
         CALL TODHMS (REAL(CLTIME), TDHMS)
         WRITE (MSGTXT,1080) CLSTA, CLSRC, TDHMS(1), TDHMS(2), TDHMS(3),
     *      TDHMS(4)
         CALL MSGWRT (6)
         END IF
      DELTIM = 86400.0D0 * (CLTIME - IMTIME)
      CALL PSHIFT (DELTIM, IMPOLY, NPOLY-1)
      CALL DPCOPY (NPOLY, IMPOLY, GEODLY)
      DDISP(1) = TDD(1)
      DDISP(2) = TDD(2)
      DISP(1)  = TDI(1) + (CLTIME - IMTIME) * DDISP(1) * 86400.
      DISP(2)  = TDI(2) + (CLTIME - IMTIME) * DDISP(2) * 86400.
      IRET = 0
      GO TO 999
C
 900  CALL MSGWRT (6)
C
 910  CALL TODHMS (REAL(CLTIME), TDHMS)
      WRITE (MSGTXT,1910) CLSTA, CLSRC, CLARR, CLFQID, TDHMS(1),
     *   TDHMS(2), TDHMS(3), TDHMS(4)
      CALL MSGWRT (6)
      IRET = -1
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, IIMRNO
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('GETDEL: Using previous IM entry stn',I3,' src',I4,' at',
     *   I4,'/',2(I2.2,':'),I2.2)
 1910 FORMAT ('GETDEL: No IM entry stn',I3,' src',I4,' sub',I3,' FQ',
     *   I3,' at',I4,'/',2(I2.2,':'),I2.2)
 1990 FORMAT ('GETDEL: ERROR ', I3, ' READING IM TABLE ROW',I9)
      END
