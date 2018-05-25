      SUBROUTINE IMSEL (INDSK, INCNO, OUTDSK, OUTCNO, VERS, INCB, OUTCB,
     *   INLUN, OUTLUN, BPOL, EPOL, BIF, EIF, FREQID, START, FINISH,
     *   NUMSRC, SOURCS, ANTENS, NUMANT, INSUB, OUTSUB, INBUF, OUTBUF,
     *   IRET)
C-----------------------------------------------------------------------
C! Copies an IM table with data selection
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2011
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
C   Copies data from selected IFs that lie within a specified time
C   range from one interferometer model (IM) table to another. Data may
C   also be filtered using the source ID, subarray number, or antenna
C   number.
C
C   If FREQID is positive then only data for frequency ID FREQID or
C   frequency ID zero  will be copied to the output table and these data
C   will be assigned frequency ID 1. If FREQID is not positive then data
C   will not be restricted by frequency ID and frequency IDs will be
C   unchanged in the output table.
C
C   If NUMSRC is positive then only data for sources listed in the
C   first NUMSRC elements of SOURCS or for source ID zero will be copied
C   to the output table. If NUMSRC is not positive then data will not be
C   restricted by source.
C
C   If NUMANT is positive then only data for antennas listed in the
C   first NUMANT elements of ANTENS or for antenna number zero will be
C   copied to the output table. If NUMANT is not positive then data will
C   not be restricted by antenna.
C
C   If INSUB is positive then only data for subarray INSUB or subarray
C   zero will be copied to the output table. If INSUB is not positive
C   then data will not be restricted by subarray.
C
C   If OUTSUB is positive then all data written to the output table
C   will be assigned to subarray OUTSUB. If OUTSUB is not positive then
C   subarray numbers will not be changed in the output table.
C
C   Writes an informational message at priority level 3 if the data are
C   copied successfully. Writes at least one error message at priority
C   level 6 or higher if the data are not copied successfully.
C
C   The input and output tables must be attached to different files.
C
C   Inputs:
C      INDSK    I        Disk number for input table
C      INCNO    I        Catalogue number for input table
C      OUTDSK   I        Disk number for output table
C      OUTCNO   I        Catalogue number for output table
C      VERS     I        Version number of input and output tables; if
C                        0 the highest version number of an IM table
C                        attached to the input file will be used
C      INCB     I(256)   Catalogue block of parent data file for
C                           the input table
C      INLUN    I        AIPS LUN used for input table
C      OUTLUN   I        AIPS LUN used for output table
C      BIF      I        Lowest numbered IF to copy
C      EIF      I        Highest numbered IF to copy
C      FREQID   I        Selected frequency ID (see above)
C      START    D        Earliest time to copy (days)
C      FINISH   D        Latest time to copy (days)
C      NUMSRC   I        Number of sources selected (see above)
C      SOURCS   I(*)     List of source IDs selected (see above)
C      ANTENS   I(*)     List of antenna numbers selected
C      NUMANT   I        Number of antennas selected (see above)
C      INSUB    I        Subarray number selected (see above)
C      OUTSUB   I        Output subarray number (see above)
C
C   Input/Output:
C      OUTCB    I(256)   Catalogue block of parent data file for
C                           the output table
C      INBUF    I(512)   Buffer for input table
C      OUTBUF   I(512)   Buffer for output table
C
C   Output:
C      IRET         I            Return status:
C                                  0 if selected data copied;
C                                  non-zero if not all data copied
C-----------------------------------------------------------------------
      INTEGER   INDSK, INCNO, OUTDSK, OUTCNO, VERS, INCB(256),
     *   OUTCB(256), INLUN, OUTLUN, BPOL, EPOL, BIF, EIF, FREQID,
     *   NUMSRC, SOURCS(*), ANTENS(*), NUMANT, INSUB, OUTSUB, INBUF(*),
     *   OUTBUF(*), IRET
      DOUBLE PRECISION START, FINISH
C
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     INVER     input table version number
C     INROW     input table row number
C     INCIDX    input row column index array
C     INCDIM    input row column dimensions array
C     OBSCOD    observing code
C     RDATE     reference date
C     NUMSTK    number of Stokes parameters
C     STK1      first Stokes parameter code
C     INNIF     number of IFs in input table
C     NUMCHN    number of channels
C     REFFRQ    reference frequency
C     CHANBW    channel bandwidth
C     REFPIX    reference pixel on frequency axis
C     NUMPOL    number of polarizations in table
C     NUMPLY    number of polynomial terms in table
C     CORREV    correlator software revision code
C
C     NUMROW    number of rows in input table
C
C     LOIF      first IF to copy
C     HIIF      last IF to copy
C     OUTNIF    number of IFs to copy
C
C     REFORM    is output data reformatted rather than copied?
C
C     OUTVER    output table version number
C     OUTROW    output row number
C     OUCIDX    output column index array
C     OUCDIM    output column dimensions array
C
C     ROW       number of rows from input file examined so far
C
C     MAXPLY    maximum number of terms in delay rate polynomials
C
C     TIME      reference time of current row
C     TIMINT    time interval covered by current row
C     SOURID    source ID of current row
C     ANTNUM    antenna number of current row
C     SUBARR    subarray number of current row
C     FQID      frequency ID of current row
C     IFR       ionospheric Faraday rotation
C     INFVR     input frequency offsets
C     INPD      input phase delay polynomials
C     GDELAY    group delay polynomials
C     INPR      input phase rate polynomials
C     GRATE     group rate polynomials
C     DISP      dispersive delay
C     DDISP     rate of change of dispersive delay
C
C     SRCSEL    is current source selected?
C     ANTSEL    is current antenna selected?
C     SRC       index into SOURCS
C     ANT       index into ANTENS
C
C     POL       polarization index
C     INBND     input band index
C     OUTBND    output band index
C     TERM      polynomial term
C     OUTFVR    output frequency offsets
C     OUTPD     output phase delay polynomials
C     OUTPR     output phase rate polynomials
C
C     IRET1     disposable return status
C
      INTEGER   INVER
      INTEGER   INROW
      INTEGER   INCIDX(20)
      INTEGER   INCDIM(20)
      CHARACTER OBSCOD*8
      CHARACTER RDATE*8
      INTEGER   NUMSTK
      INTEGER   STK1
      INTEGER   INNIF
      INTEGER   NUMCHN
      DOUBLE PRECISION REFFRQ
      DOUBLE PRECISION CHANBW
      DOUBLE PRECISION REFPIX
      INTEGER   NUMPOL
      INTEGER   NUMPLY
      DOUBLE PRECISION CORREV
C
      INTEGER   NUMROW
C
      INTEGER   LOIF
      INTEGER   HIIF
      INTEGER   OUTNIF
C
      LOGICAL   REFORM
C
      INTEGER   OUTVER
      INTEGER   OUTROW
      INTEGER   OUCIDX(20)
      INTEGER   OUCDIM(20)
C
      INTEGER   ROW
C
      INTEGER   MAXPLY
      PARAMETER (MAXPLY = 20)
C
      DOUBLE PRECISION TIME
      REAL      TIMINT
      INTEGER   SOURID
      INTEGER   ANTNUM
      INTEGER   SUBARR
      INTEGER   FQID
      REAL      IFR
      REAL      INFVR(MAXIF)
      DOUBLE PRECISION INPD(2, MAXIF, MAXPLY)
      DOUBLE PRECISION GDELAY(2, MAXPLY)
      DOUBLE PRECISION INPR(2, MAXIF, MAXPLY)
      DOUBLE PRECISION GRATE(2, MAXPLY)
      REAL      DISP
      REAL      DDISP
C
      LOGICAL   SRCSEL
      LOGICAL   ANTSEL
      INTEGER   SRC
      INTEGER   ANT
C
      INTEGER   POL, LBPOL, NEWPOL, K
      INTEGER   INBND
      INTEGER   OUTBND
      INTEGER   TERM
      REAL      OUTFVR(MAXIF)
      DOUBLE PRECISION OUTPD(2, MAXIF, MAXPLY)
      DOUBLE PRECISION OUTPR(2, MAXIF, MAXPLY)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      INVER = VERS
      CALL IMINIT ('READ', INBUF, INDSK, INCNO, INVER, INCB, INLUN,
     *   INROW, INCIDX, INCDIM, OBSCOD, RDATE, NUMSTK, STK1, INNIF,
     *   NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY, CORREV, IRET)
      IF (IRET.EQ.0) THEN
         NUMROW = INBUF(5)
C                                       Set IF range:
         LOIF = MAX (1, BIF)
         HIIF = MIN (INNIF, EIF)
         OUTNIF = HIIF - LOIF + 1
         IF (OUTNIF.LE.0) THEN
            OUTNIF = INNIF
            LOIF = 1
            END IF
C                                       set pol range
         LBPOL = MAX (1, BPOL)
         NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
         IF (NEWPOL.LE.0) THEN
            NEWPOL = NUMPOL
            LBPOL = 1
            END IF
         REFORM = (OUTNIF.NE.INNIF) .OR. (NEWPOL.NE.NUMPOL)
C
         OUTVER = INVER
         CALL IMINIT ('WRIT', OUTBUF, OUTDSK, OUTCNO, OUTVER, OUTCB,
     *      OUTLUN, OUTROW, OUCIDX, OUCDIM, OBSCOD, RDATE, NUMSTK, STK1,
     *      OUTNIF, NUMCHN, REFFRQ, CHANBW, REFPIX, NEWPOL, NUMPLY,
     *      CORREV, IRET)
         IF (IRET.EQ.0) THEN
            ROW = 0
C
C           Invariant: IRET = 0 implies that selected data from the
C                      first ROW rows of the input table has been
C                      written to the output table and REFORM = false
C                      implies that all of the data in the first ROW
C                      rows has been copied
C           Bound: NUMROW - ROW when IRET = 0; 0 otherwise
C
 10         IF ((IRET.EQ.0) .AND. (ROW.NE.NUMROW)) THEN
               INROW = ROW + 1
               CALL TABIM ('READ', INBUF, INROW, INCIDX, INCDIM, NUMPOL,
     *            TIME, TIMINT, SOURID, ANTNUM, SUBARR, FQID, IFR,
     *            INFVR, INPD, GDELAY, INPR, GRATE, DISP, DDISP, IRET)
               IF (IRET.EQ.0) THEN
C                                       check source
                  IF ((NUMSRC.GT.0) .AND. (SOURID.NE.0)) THEN
                     SRCSEL = .FALSE.
                     DO 20 SRC = 1,NUMSRC
                        IF (SOURCS(SRC).EQ.SOURID) SRCSEL = .TRUE.
 20                     CONTINUE
                  ELSE
                     SRCSEL = .TRUE.
                     END IF
C                                       check ant
                  IF ((NUMANT.GT.0) .AND. (ANTNUM.NE.0)) THEN
                     ANTSEL = .FALSE.
                     DO 30 ANT = 1,NUMANT
                        IF (ANTENS(ANT).EQ.ANTNUM) ANTSEL = .TRUE.
 30                     CONTINUE
                  ELSE
                     ANTSEL = .TRUE.
                     END IF
C
                  IF ((START.LE.TIME) .AND. (TIME.LE.FINISH) .AND.
     *               ((FREQID.LE.0) .OR. (FQID.EQ.0) .OR.
     *               (FQID.EQ.FREQID)) .AND. SRCSEL .AND. ANTSEL .AND.
     *               ((INSUB.LE.0) .OR. (SUBARR.LE.0) .OR.
     *               (SUBARR.EQ.INSUB))) THEN
C
C                    This record is selected so copy the data to
C                    the output table:
C
                     DO 60 POL = 1,NEWPOL
                        K = POL + LBPOL - 1
                        DO 50 INBND = LOIF, HIIF
                           OUTBND = INBND - LOIF + 1
                           OUTFVR(OUTBND) = INFVR(INBND)
                           DO 40 TERM = 1,NUMPLY
                              OUTPD(POL,OUTBND,TERM) =
     *                           INPD(K,INBND,TERM)
                              OUTPR(POL,OUTBND,TERM) =
     *                           INPR(K,INBND,TERM)
   40                         CONTINUE
   50                      CONTINUE
   60                   CONTINUE
                     IF (FREQID.GT.0) FQID = 1
                     IF (OUTSUB.GT.0) SUBARR = OUTSUB
                     CALL TABIM ('WRIT', OUTBUF, OUTROW, OUCIDX, OUCDIM,
     *                  NEWPOL, TIME, TIMINT, SOURID, ANTNUM, SUBARR,
     *                  FQID, IFR, OUTFVR, OUTPD, GDELAY, OUTPR, GRATE,
     *                  DISP, DDISP,  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT, 9060) IRET
                        CALL MSGWRT (6)
                        END IF
C                    This record is not selected so output table will
C                    not be an exact copy of the input table.
                  ELSE
                     REFORM = .TRUE.
                     END IF
C                 This record is flagged and will not be copied so the
C                 output table will not be an exact copy of the input
C                 table.
               ELSE IF (IRET.LT.0) THEN
                  REFORM = .TRUE.
                  IRET = 0
C                                       read failed
               ELSE
                  WRITE (MSGTXT, 9061) IRET
                  CALL MSGWRT (6)
                  END IF
C
               ROW = ROW + 1
               GO TO 10
               END IF
C
            CALL TABIM ('CLOS', OUTBUF, OUTROW, OUCIDX, OUCDIM, NEWPOL,
     *         TIME, TIMINT, SOURID, ANTNUM, SUBARR, FQID, IFR, OUTFVR,
     *         OUTPD, GDELAY, OUTPR, GRATE, DISP, DDISP, IRET1)
            IF (IRET1.NE.0) THEN
               WRITE (MSGTXT, 9062) IRET1
               CALL MSGWRT (6)
               IRET = IRET1
               END IF
C                                       failed to open output table
         ELSE
            WRITE (MSGTXT, 9063) IRET
            CALL MSGWRT (6)
            END IF
C
         CALL TABIM ('CLOS', INBUF, INROW, INCIDX, INCDIM, NUMPOL, TIME,
     *         TIMINT, SOURID, ANTNUM, SUBARR, FQID, IFR, INFVR,
     *         INPD, GDELAY, INPR, GRATE, DISP, DDISP, IRET1)
         IF (IRET1.NE.0) THEN
            WRITE (MSGTXT, 9064) IRET1
            CALL MSGWRT (6)
            IRET = IRET1
            END IF
      ELSE
C
C        Failed to open input table
C
         WRITE (MSGTXT, 9065) IRET
         CALL MSGWRT (6)
         END IF
C
      IF (IRET.EQ.0) THEN
C
C        Issue summary message:
C
         IF (REFORM) THEN
            WRITE (MSGTXT, 1060) INDSK, INCNO, INVER, OUTDSK, OUTCNO,
     *                           OUTVER
         ELSE
            WRITE (MSGTXT, 1061) INDSK, INCNO, INVER, OUTDSK, OUTCNO,
     *                           OUTVER
         END IF
         CALL MSGWRT (3)
         END IF
C-----------------------------------------------------------------------
 1060 FORMAT ('Reformatted IM file from vol/cno/vers ', I2, I5, I4,
     *        ' to ', I2, I5, I4)
 1061 FORMAT ('Copied IM file from vol/cno/vers ', I2, I5, I4, ' to ',
     *        I2, I5, I4)
 9060 FORMAT ('IMSEL: ERROR ', I3, ' WRITING TO NEW TABLE')
 9061 FORMAT ('IMSEL: ERROR ', I3, ' READING FROM OLD TABLE')
 9062 FORMAT ('IMSEL: ERROR ', I3, ' CLOSING NEW TABLE')
 9063 FORMAT ('IMSEL: ERROR ', I3, ' OPENING NEW TABLE')
 9064 FORMAT ('IMSEL: ERROR ', I3, ' CLOSING OLD TABLE')
 9065 FORMAT ('IMSEL: ERROR ', I3, ' OPENING OLD TABLE')
      END
