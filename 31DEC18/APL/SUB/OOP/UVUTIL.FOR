C   UVDATA Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "UVDATA" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2004-2007, 2009-2010, 2012-2013, 2015, 2017
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
C   Public functions:
C
C   AVGVIS (naxis, d2, d3, d4, d5, lim, visin, visout)
C      Vector average a visibility record
C   SCNAVG (name, source, qual, timer, maxant, vis, ierr)
C      Vector average a specified set of data
C   SCNHAY (name, source, qual, timer, maxant, vis, ampfrc, ierr)
C      Extract scan information needed for making Haystack HF tables.
C   NXTAVG (opcode, name, interv, avgif, avgpol, antwt, uvrang, uvwt,
C      maxan, maxifs, maxpol, timec, timei, sid, fqid, vis, ierr)
C      Return all data averaged for the next time interval.
C   UVCOPY (uvin, uvout, ierr)
C      Copies a uv data object to another with application of any
C      calibration editing, and selection criteria 1 subarray at a time
C   UVCOP1 (uvin, uvout, ierr)
C      Copies a uv data object (only 1 subarray) to another with
C      application of any calibration editing, and selection criteria.
C   CP2SCR (uvdata, uvscr, ierr)
C      copies a uv data object to scratch file looping over sub-array
C      and applying calibration, editing, and selection criteria.  (This
C      is needed only if there is polarization cal to be applied.)
C   UV2SCR (uvdata, uvscr, ierr)
C      Copies a uv data object to a scratch object with application of
C      any calibration editing, and selection criteria.
C   UVRSCR (uvdata, uvscr, cmpscr, ierr)
C      Copies a uv data object to a scratch object with no data
C      selection or calibration or tables copied.
C   UVCLIP (uvin, uvout, nflag, ierr)
C      Copy uvdata flagging data with excessive amplitudes.
C   UVVCLP (uvin, uvout, nflag, ierr)
C      Copy uv data flagging by excessive VPOL.
C   U2IDES (uvdata, image, dodft, ierr)
C      Copies descriptive info from a Uvdata object to an image and
C      initializes the image descriptors.
C   UVFRQS (uvdata, uvfreq, freqs, ierr)
C      Return reference frequency and an array of frequencies for each
C      channel/IF.
C   UVREFQ (uvdata, chtype, refreq, refpix, ierr)
C      Determines reference frequency and pixel for a uvdata set subject
C      to frequency selection and averaging.
C   UVDTCO (uvin, uvout, ierr)
C      Copy descriptive tables (AN, FQ, SU) from uvin to uvout.
C   UVTCOP (uvin, uvout, type, ver, ierr)
C      Copy a specified table form one uvdata to another, ver=0 => all.
C   UVFCOP (uvin, uvout, ierr)
C      Copy portion of FQ table given by input uvdata selection criteria
C      to another uvdata.
C   UV2TAB (uvdata, table, tbtype, tbver, ierr)
C      Makes table object associated with a uvdata object.
C   UVBAVG (uvin, uvout, ierr)
C      Baseline dependent time averaging of a uv data set.
C   UVTAVG (uvin, uvout, ierr)
C      Time average time sorted data.
C   UVCALT (uvin, uvout, ierr)
C      Copies all tables from one uvdata object to another.
C   UVAMPS (uvin, uvran, namps, amps, ierr)
C     Determine range of baselines and an array of average amplitude
C     vs. baseline length.
C   UVFRSC (uvname, snname, ierr)
C      Faraday rotation self cal solutions.
C
C  OOA Fronts to AIPS uv data specific routines:
C   SLFCAL (uvname, snname, ierr)
C      Does self cal solutions
C   SLFREF (snname, refant, ierr)
C      Refererence all phases to a common reference antenna.
C   SLFSMO (uvname, snname, isuba, ierr)
C      Smooths an SN table, interpolating failed solutions
C
C  OOA Front private routines
C   SLFPA (vobs, maxan, maxifs, maxpol, numif, refant, avgif, avgpol,
C      dol1, dogcon, gaerr, mode, minno, confac, snrmin, prtlv, fflim,
C      fflast, creal, cimag, cwt, refan, gotant)
C      Compute self calibration solutions from averaged data.
C   SLFSOU (snname, numan, numif, numpol, timec, timei, suba, sid, fqid,
C      creal, cimag, cwt, refan, gotant, summgm, cntmgm, ierr)
C      Write solution to SN object.
C   FRSOLN (vobs, maxan, maxifs, maxpol, numif, avgif, snrmin, ratmin,
c      ratmax, prtlv, creal, cimag, cwt, refan, gotant)
C      Compute Faraday rotation solutions.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXVS, BIGVIS
C                                       MXVS = maximum no. correlations
C                                       in a record.
      PARAMETER (MXVS = MAXCIF)
C                                       BIGVIS = size of vis array
      PARAMETER (BIGVIS = 1000000)
C                                        Local Info for uv util.
      REAL     RP(50), VS(3,MXVS), BVIS(BIGVIS)
      COMMON /UVULCM/ RP, VS, BVIS
LOCAL END
      SUBROUTINE AVGVIS (D2, D3, D4, D5, LIM, VISIN, VISOUT)
C-----------------------------------------------------------------------
C   Vector weighted average a visibility record, does up to 5 axes
C   including the complex axis.
C   Inputs:
C      D2      I      Second dimension. min = 1
C      D3      I      Third dimension. min = 1
C      D4      I      Fourth dimension. min = 1
C      D5      I      Fifth dimension. min = 1
C      LIM     I(2,5) Low and high limits on each axis.  should be zero
C                     filled past last valid data.
C      VISIN   R(3,*) Input visibility array
C   Outputs:
C      VISOUT  R(3)   Averaged visibility.
C-----------------------------------------------------------------------
      INTEGER   D2, D3, D4, D5, LIM(2,5)
      REAL      VISIN(3,D2,D3,D4,D5), VISOUT(3)
C
      INTEGER   I2, I3, I4, I5
      REAL      SUMRE, SUMIM, SUMWT, WT
C-----------------------------------------------------------------------
      SUMRE = 0.0
      SUMIM = 0.0
      SUMWT = 0.0
      DO 50 I5 =  LIM(1,5), LIM(2,5)
         DO 40 I4 =  LIM(1,4), LIM(2,4)
            DO 30 I3 =  LIM(1,3), LIM(2,3)
               DO 20 I2 =  LIM(1,2), LIM(2,2)
                  IF (VISIN(3,I2,I3,I4,I5).GT.0.0) THEN
                     WT = VISIN(3,I2,I3,I4,I5)
                     SUMRE = SUMRE + VISIN(1,I2,I3,I4,I5) * WT
                     SUMIM = SUMIM + VISIN(2,I2,I3,I4,I5) * WT
                     SUMWT = SUMWT + WT
                     END IF
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C                                       Average
      IF (SUMWT.GT.1.0E-20) THEN
         VISOUT(1) = SUMRE / SUMWT
         VISOUT(2) = SUMIM / SUMWT
         VISOUT(3) = SUMWT
      ELSE
         VISOUT(1) = 0.0
         VISOUT(2) = 0.0
         VISOUT(3) = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE SCNAVG (NAME, SOURCE, QUAL, TIMER, MAXNT, VIS, IERR)
C-----------------------------------------------------------------------
C   Vector average a specified set of data.
C   Inputs:
C      NAME    C*?    UV data object name
C      SOURCE  C*16   Source name
C      QUAL    I      Qualifier.
C      TIMER   R(2)   Start and stop time in days.
C      MAXNT  I      dimension of VIS.
C   Outputs:
C      VIS     R(2,maxant,maxant)  First entry (1,?,?):
C                     Real part in upper half (1,i,j) i<j.
C                     Imaginary part in lower half.
C                     Second entry (2,?,?):
C                     Weight in upper part, count in lower
C      IERR    I      Error code, 0=> OK, 1= no data found. else error.
C-----------------------------------------------------------------------
      INTEGER   QUAL, MAXNT, IERR
      CHARACTER NAME*(*), SOURCE*16
      REAL      TIMER(2), VIS(2,MAXNT,MAXNT)
C
      INTEGER   LOOP, I1, I2, TYPE, DIM(7), LIMS(2,7), D(7), NAXIS(7),
     *   NDIM, ANT1, ANT2, INDXB, DUMMY, INDXA1, INDXA2
      CHARACTER CSOU(30)*16, CDUMMY*1, UVTYPE*2
      REAL      TR(8), VT(3)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Add selection criteria to NAME
C                                       Source name.
      CSOU(1) = SOURCE
      DO 30 LOOP = 2,30
         CSOU(LOOP) = '   '
 30      CONTINUE
      DIM(1) = 16
      DIM(2) = 30
      DIM(3) = 0
      CALL SECPUT (NAME, 'SOURCS', OOACAR, DIM, DUMMY, CSOU, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       qualifier
      DIM(1) = 1
      DIM(2) = 1
      CALL SECPUT (NAME, 'SELQUA', OOAINT, DIM, QUAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Timerange
      DIM(1) = 8
      CALL RFILL (8, 0, TR)
      TR(1) = TIMER(1)
      TR(5) = TIMER(2)
      CALL SECPUT (NAME, 'TIMRNG', OOARE, DIM, TR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Init output
      DO 20 I2 = 1,MAXNT
         DO 10 I1 = 1,MAXNT
            VIS(1,I1,I2) = 0.0
            VIS(2,I1,I2) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Open object
      CALL OUVOPN (NAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (NAME, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get vis info
      CALL UVDGET (NAME, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (UVTYPE(1:1).EQ.'S') THEN
         CALL UVDFND (NAME, 1, 'BEAM', INDXB, IERR)
      ELSE
         CALL UVDFND (NAME, 1, 'BASELINE', INDXB, IERR)
         IF (IERR.NE.0) THEN
            INDXB = -1
            CALL UVDFND (NAME, 1, 'ANTENNA1', INDXA1, IERR)
            IF (IERR.EQ.0) CALL UVDFND (NAME, 1, 'ANTENNA2', INDXA2,
     *         IERR)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE'
         GO TO 990
         END IF
      CALL FILL (5, 1, D)
      CALL FILL (10, 0, LIMS)
      DO 50 LOOP = 1,NDIM
         D(LOOP) = NAXIS(LOOP)
         LIMS(1,LOOP) = 1
         LIMS(2,LOOP) = NAXIS(LOOP)
 50      CONTINUE
C                                       Loop reading, averaging data
 100     CALL UVREAD (NAME, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Done?
         IF (IERR.LT.0) GO TO 500
C                                       Crack baseline
         IF (INDXB.GT.0) THEN
            ANT1 = (RP(INDXB) / 256.0) + 0.001
            ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(INDXA1) + 0.1
            ANT2 = RP(INDXA2) + 0.1
            END IF
         IF ((UVTYPE(1:1).EQ.'S') .AND. (ANT1.EQ.0)) ANT1 = ANT2
C                                       Ignore antennas with too large
C                                       values.
         IF ((ANT1.LE.0) .OR. (ANT1.GT.MAXNT)) GO TO 100
         IF ((ANT2.LE.0) .OR. (ANT2.GT.MAXNT)) GO TO 100
C                                       Average vis
         CALL AVGVIS (D(2), D(3), D(4), D(5), LIMS, VS, VT)
C                                       Update accumulation
         IF (VT(3).GT.0.0) THEN
C                                       Could have auto correlations
            VIS(1,ANT2,ANT1) = VIS(1,ANT2,ANT1) + VT(2) * VT(3)
            VIS(1,ANT1,ANT2) = VIS(1,ANT1,ANT2) + VT(1) * VT(3)
            VIS(2,ANT1,ANT2) = VIS(2,ANT1,ANT2) + VT(3)
            VIS(2,ANT2,ANT1) = VIS(2,ANT2,ANT1) + 1.0
            END IF
C                                       Loop until done
         GO TO 100
C                                       Close
 500  CALL OUVCLO (NAME, IERR)
      IF (IERR.GT.0) GO TO 995
C                                       Normalize by weight sum
      DO 520 I1 = 1,MAXNT-1
         DO 510 I2 = I1,MAXNT
            IF (VIS(2,I2,I1).GT.0.5) THEN
               VIS(1,I1,I2) = VIS(1,I1,I2) / VIS(2,I1,I2)
               IF (I1.NE.I2) VIS(1,I2,I1) = VIS(1,I2,I1) / VIS(2,I1,I2)
               END IF
 510        CONTINUE
 520     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ERROR AVERAGING SCAN FOR ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE SCNHAY (NAME, SOURCE, QUAL, TIMER, MAXAN, MXIF,
     *   VIS, AMPFRC, UVAVG, MAXSMP, IERR)
C-----------------------------------------------------------------------
C   Extract scan information needed for making Haystack HF tables.
C   Inputs:
C      NAME    C*?    UV data object name
C      SOURCE  C*16   Source name
C      QUAL    I      Qualifier.
C      TIMER   R(2)   Start and stop time in days.
C      MAXAN   I      dimension of VIS.
C      MXIF    I      Maximum number of IFs
C   Outputs:
C      VIS     R(2,mxif,maxant,maxant)  First entry (1,if,?,?):
C                     Real part in upper half (1,if,i,j) i<j. 1/IF
C                     Imaginary part in lower half. 1/IF
C                     Second entry (2,if,?,?):
C                     Weight in upper part, count in lower, 1/IF
C      AMPFRC  R(2,maxant,maxant)  First entry (1,?,?)
C                     Upper half = scalar average amplitude
C                     lower half = IF averaged number of time samples
C                                  for baseline.
C                     Second entry (2,?,?):
C                     Upper half = real part of vector average
C                     Lower half = imaginary part of vector average.
C      MAXSMP  I      The maximum number of time samples on any
C                     baseline/IF.
C      UVAVG   R(2,maxant,maxant)  Upper half: (1,i,j) = u, (2,i,j) = v
C      IERR    I      Error code, 0=> OK, 1= no data found. else error.
C-----------------------------------------------------------------------
      INTEGER   QUAL, MAXAN, MXIF, MAXSMP, IERR
      CHARACTER NAME*(*), SOURCE*16
      REAL      TIMER(2), VIS(2,MXIF,MAXAN,MAXAN),
     *   AMPFRC(2,MAXAN,MAXAN), UVAVG(2,MAXAN,MAXAN)
C
      INTEGER   LOOP, I1, I2, I3, TYPE, DIM(7), LIMS(2,7), D(7),
     *   NAXIS(7), NDIM, ANT1, ANT2, INDXB, INDXU, INDXV, INDXIF, NIF,
     *   SCLCNT, DUMMY, INDXA1, INDXA2
      CHARACTER CSOU(30)*16, CDUMMY*1, UVTYPE*2
      REAL      TR(8), VT(3), SUMRE, SUMIM, SUMCNT, RMAXC
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Add selection criteria to NAME
C                                       Source name.
      CSOU(1) = SOURCE
      DO 100 LOOP = 2,30
         CSOU(LOOP) = ' '
 100     CONTINUE
      DIM(1) = 16
      DIM(2) = 30
      DIM(3) = 0
      CALL SECPUT (NAME, 'SOURCS', OOACAR, DIM, DUMMY, CSOU, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       qualifier
      DIM(1) = 1
      DIM(2) = 1
      CALL SECPUT (NAME, 'SELQUA', OOAINT, DIM, QUAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Timerange
      DIM(1) = 8
      CALL RFILL (8, 0, TR)
      TR(1) = TIMER(1)
      TR(5) = TIMER(2)
      CALL SECPUT (NAME, 'TIMRNG', OOARE, DIM, TR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open object
      CALL OUVOPN (NAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (NAME, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (UVTYPE(1:1).NE.'U') THEN
         IERR = 8
         MSGTXT = 'SCNHAY DOES NOT WORK FOR ''' // UVTYPE // '''DATA'
         GO TO 990
         END IF

C                                       Get vis info
      CALL UVDGET (NAME, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDFND (NAME, 1, 'BASELINE', INDXB, IERR)
      IF (IERR.NE.0) THEN
         INDXB = -1
         CALL UVDFND (NAME, 1, 'ANTENNA1', INDXA1, IERR)
         IF (IERR.EQ.0) CALL UVDFND (NAME, 1, 'ANTENNA2', INDXA2,
     *      IERR)
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE'
         GO TO 990
         END IF
      CALL UVDFND (NAME, 1, 'UU-L', INDXU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
         GO TO 990
         END IF
      CALL UVDFND (NAME, 1, 'VV-L', INDXV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
         GO TO 990
         END IF
      CALL UVDFND (NAME, 2, 'IF  ', INDXIF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING IF AXIS'
         GO TO 990
         END IF
      IF (IERR.NE.0) GO TO 995
      NIF = NAXIS(INDXIF)
C                                       Init output
      DO 30 I3 = 1,NIF
         DO 20 I2 = 1,MAXAN
            DO 10 I1 = 1,MAXAN
               VIS(1,I3,I1,I2) = 0.0
               VIS(2,I3,I1,I2) = 0.0
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
      DO 40 I2 = 1,MAXAN
         DO 50 I1 = 1,MAXAN
            AMPFRC(1,I1,I2) = 0.0
            AMPFRC(2,I1,I2) = 0.0
            UVAVG(1,I1,I2) = 0.0
            UVAVG(2,I1,I2) = 0.0
 50         CONTINUE
 40      CONTINUE
C                                       Set axis arrays
      CALL FILL (5, 1, D)
      CALL FILL (10, 0, LIMS)
      DO 110 LOOP = 1,NDIM
         D(LOOP) = NAXIS(LOOP)
         LIMS(1,LOOP) = 1
         LIMS(2,LOOP) = NAXIS(LOOP)
 110     CONTINUE
C                                       Loop reading, averaging data
 200     CALL UVREAD (NAME, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Done?
         IF (IERR.LT.0) GO TO 500
C                                       Crack baseline
         IF (INDXB.GT.0) THEN
            ANT1 = (RP(INDXB) / 256.0) + 0.001
            ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(INDXA1) + 0.1
            ANT2 = RP(INDXA2) + 0.1
            END IF
C                                       Ignore antennas with too large
C                                       values.
         IF ((ANT1.LE.0) .OR. (ANT1.GT.MAXAN)) GO TO 200
         IF ((ANT2.LE.0) .OR. (ANT2.GT.MAXAN)) GO TO 200
C                                       Accumulate u,v
         UVAVG(1,ANT1,ANT2) = UVAVG(1,ANT1,ANT2) + RP(INDXU)
         UVAVG(2,ANT1,ANT2) = UVAVG(2,ANT1,ANT2) + RP(INDXV)
         UVAVG(1,ANT2,ANT1) = UVAVG(1,ANT2,ANT1) + 1.0
C                                       Average vis
         DO 300 I3 = 1,NIF
C                                       Don't average in IF
            LIMS(1,INDXIF) = I3
            LIMS(2,INDXIF) = I3
            CALL AVGVIS (D(2), D(3), D(4), D(5), LIMS, VS, VT)
C                                       Update accumulation
            IF (VT(3).GT.0.0) THEN
               VIS(1,I3,ANT2,ANT1) = VIS(1,I3,ANT2,ANT1) + VT(2) * VT(3)
               VIS(1,I3,ANT1,ANT2) = VIS(1,I3,ANT1,ANT2) + VT(1) * VT(3)
               VIS(2,I3,ANT1,ANT2) = VIS(2,I3,ANT1,ANT2) + VT(3)
               VIS(2,I3,ANT2,ANT1) = VIS(2,I3,ANT2,ANT1) + 1.0
               AMPFRC(1,ANT1,ANT2) = AMPFRC(1,ANT1,ANT2) +
     *            SQRT (VT(1)*VT(1)+VT(2)*VT(2)) * VT(3)
               AMPFRC(1,ANT2,ANT1) = AMPFRC(1,ANT2,ANT1) + 1.0
               END IF
 300        CONTINUE
C                                       Loop until done
         GO TO 200
C                                       Close
 500  CALL OUVCLO (NAME, IERR)
      IF (IERR.GT.0) GO TO 995
C                                       Normalize by element count
      RMAXC = -1.0
      DO 550 I1 = 1,MAXAN-1
         DO 540 I2 = I1,MAXAN
C                                       U, V average
            IF (UVAVG(1,I2,I1).GT.0.0) THEN
               UVAVG(1,I1,I2) = UVAVG(1,I1,I2) / UVAVG(1,I2,I1)
               UVAVG(2,I1,I2) = UVAVG(2,I1,I2) / UVAVG(1,I2,I1)
               END IF
C                                       Acumulate info
            SUMRE = 0.0
            SUMIM = 0.0
            SUMCNT = 0.0
            SCLCNT = 0
            DO 530 I3 = 1,NIF
               IF (VIS(2,I3,I2,I1).GT.0.5) THEN
                  SUMRE = SUMRE + VIS(1,I3,I1,I2)
                  SUMIM = SUMIM + VIS(1,I3,I2,I1)
                  SUMCNT = SUMCNT + VIS(2,I3,I1,I2)
                  VIS(1,I3,I1,I2) = VIS(1,I3,I1,I2) / VIS(2,I3,I1,I2)
                  IF (I1.NE.I2)
     *               VIS(1,I3,I2,I1) = VIS(1,I3,I2,I1) / VIS(2,I3,I1,I2)
                  SCLCNT = SCLCNT + 1
                  RMAXC = MAX (RMAXC, VIS(2,I3,I2,I1))
                  END IF
 530           CONTINUE
C                                       Scalar amp average.
            IF (AMPFRC(1,I2,I1).GT.0.0) THEN
               AMPFRC(1,I1,I2) = AMPFRC(1,I1,I2) / SUMCNT
               AMPFRC(1,I2,I1) = AMPFRC(1,I2,I1) / NIF
               END IF
C                                       vector average
            IF (SCLCNT.GT.0) THEN
               AMPFRC(2,I1,I2) = SUMRE / SUMCNT
               AMPFRC(2,I2,I1) = SUMIM / SUMCNT
               END IF
 540        CONTINUE
 550     CONTINUE
C                                       Maximum count
      MAXSMP = RMAXC + 0.5
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ERROR AVERAGING SCAN FOR ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE NXTAVG (OPCODE, NAME, INTERV, AVGIF, AVGPOL, ANTWT,
     *   UVRANG, WTUV, MAXAN, MAXIFS, MAXPOL, TIMEC, TIMEI, SID, FQID,
     *   VIS, IERR)
C-----------------------------------------------------------------------
C   Return weighted vector average of next INTERV data.  The first call
C   must be with OPCODE='OPEN' which will return the first interval.
C   Thereafter OPCODE should be 'READ'.  When done a call with
C   OPCODE='CLOS' is needed to close the uv data object.  A value of
C   IERR=-1 will be returned on the last set of valid data.
C      On OPCODE = 'OPEN' and AVGPOL false the desired STOKES must be
C   set on NAME.
C      The weights can be modified using antenna based as well as
C   baseline length based weighting.
C      Data is averaged until one of several conditions is met:
C   1) the time exceeds the initial time plus the specified interval.
C   2) the source id (if present) changes
C   3) the FQ id (if present) changes.
C   Data from all baselines are returned.
C   On OPEN, if INTERV <= 0, INTERV will be set to an interval found in
C   the data.
C   Inputs:
C      OPCODE  C*4    'OPEN', 'READ', 'CLOS'
C      NAME    C*?    UV data object name
C      AVGIF   L      If true average in IF
C      AVGPOL  L      If true average in polarization (Stokes 'I')
C      ANTWT   R(*)   Extra weights to antennas (>=0 => 1.0)
C      UVRANG  R(2)   Range of baseline lengths with full weight
C                     (kilolamda). 0s => all baselines
C      WTUV    R      Weight outside of UVRANG. (No default)
C      MAXAN   I      Maximum antenna number (NOT number of antennas)
C      MAXIFS  I      Maximum number of IFs
C      MAXPOL  I      Maximum number of polarizations;
C   In/out:
C      INTERV  R      Time interval in days. Will be reduced by 1 sec or
C                     0.1 of INTERV whichever is less.  <= 0 -> 10 sec
C                     except on OPEN.  There the data are read and an
C                     interval is found and stored in the object as
C                     'SOLINT'.
C   Outputs:
C      TIMEC   D      Center time of observations (days)
C      TIMEI   R      Actual time interval (days)
C      SID     I      Source Id if present else -1.
C      FQID    I      FQ id if present else -1.
C      VIS     R(2,maxif,maxpol,maxant,maxant)
C                     First entry (1,if,pol,?,?):
C                     Real part in upper half (1,if,pol,i,j) i<j.
C                     Imaginary part in lower half.
C                     Second entry (2,if,pol,?,?):
C                     Weight in upper part, count in lower, 1/IF
C      IERR    I      Error code, 0=> OK, -1=> done, returning valid
C                     data, 1= no data found. else error.
C   Common:
C      RP,VS,BVIS from UVSTUF are used.  The BVIS may be passed in as
C      VIS without a problem.  BVIS is only used while finding the
C      INTERV on OPEN.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, NAME*(*)
      LOGICAL   AVGIF, AVGPOL
      INTEGER   MAXAN, MAXIFS, MAXPOL, SID, FQID, IERR
      REAL      INTERV, ANTWT(*), UVRANG(2), WTUV, TIMEI,
     *   VIS(2,MAXIFS,MAXPOL,MAXAN,MAXAN)
      DOUBLE PRECISION TIMEC
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOP, I1, I2, I3, I4, TYPE, DIM(7), LIMS(2,7), D(7),
     *   NAXIS(7), NDIM, ANT1, ANT2, INDXB, INDXT, INDXS, INDXFQ, I,
     *   INDXIF, INDXP, INDXU, INDXV, NIF, NPOLN, TIMCNT, CSID, CFQID,
     *   DUMMY, INDXA1, INDXA2
      LOGICAL   READVS, SINGLE
      CHARACTER STOKES*4, CDUMMY*1, UVTYPE*2
      REAL      VT(3), TIMMAX, TIMMIN, STIME, CTIME, AWT(MAXANT), BL2,
     *   BLWT, UVR(2), SWT, LSTIM, ADJI
      DOUBLE PRECISION TIMSUM
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE READVS, INDXB, INDXT, INDXS, INDXFQ, INDXIF, INDXP, INDXU,
     *   INDXV, NDIM, LIMS, D, NAXIS, NIF, NPOLN, INDXA1, INDXA2
      DATA READVS /.TRUE./
      DATA INDXB, INDXT, INDXS, INDXFQ, INDXIF, INDXP /6*-1/
      DATA NIF, NPOLN /0, 0/
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OUVCLO (NAME, IERR)
         IF (IERR.GT.0) GO TO 995
         GO TO 999
         END IF
      IF (OPCODE.EQ.'OPEN') THEN
C                                       Specify desired polarizations
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         IF (AVGPOL) THEN
            STOKES = 'I'
            CALL SECPUT (NAME, 'STOKES', OOACAR, DIM, DUMMY, STOKES,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       Open object
         CALL OUVOPN (NAME, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         READVS = .TRUE.
C                                       Check data type
         CALL UVDGET (NAME, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get vis info
         CALL UVDGET (NAME, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         SINGLE = UVTYPE(1:1).EQ.'S'
         IF (SINGLE) THEN
            CALL UVDFND (NAME, 1, 'BEAM', INDXB, IERR)
         ELSE
            CALL UVDFND (NAME, 1, 'BASELINE', INDXB, IERR)
            IF (IERR.NE.0) THEN
               INDXB = -1
               CALL UVDFND (NAME, 1, 'ANTENNA1', INDXA1, IERR)
               IF (IERR.EQ.0) CALL UVDFND (NAME, 1, 'ANTENNA2', INDXA2,
     *            IERR)
               END IF
            END IF
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE'
            GO TO 990
            END IF
C                                       Time
         CALL UVDFND (NAME, 1, 'TIME', INDXT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
            GO TO 990
            END IF
C                                       U
         IF (.NOT.SINGLE) THEN
            CALL UVDFND (NAME, 1, 'UU-L', INDXU, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER U'
               GO TO 990
               END IF
C                                       V
            CALL UVDFND (NAME, 1, 'VV-L', INDXV, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER V'
               GO TO 990
               END IF
            END IF
C                                       Source, FQ id may or may not be
C                                       present.
         CALL UVDFND (NAME, 1, 'SOURCE  ', INDXS, IERR)
         CALL UVDFND (NAME, 1, 'FQID    ', INDXFQ, IERR)
C                                       IF axis
         CALL UVDFND (NAME, 2, 'IF  ', INDXIF, IERR)
C                                       May not be there.
         IF (IERR.NE.0) THEN
            INDXIF = -1
            IERR = 0
            END IF
         IF (INDXIF.GT.0) THEN
            NIF = NAXIS(INDXIF)
         ELSE
            NIF = 1
            END IF
         IF (AVGIF) NIF = 1
C                                       Check for blown arrays
         IF (NIF.GT.MAXIFS) THEN
            MSGTXT = 'NXTAVG: TOO MANY IFS'
            IERR = 5
            GO TO 990
            END IF
C                                       STOKES axis
         CALL UVDFND (NAME, 2, 'STOKES', INDXP, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING STOKES AXIS'
            GO TO 990
            END IF
         NPOLN = MIN (4, NAXIS(INDXP))
         IF (AVGPOL) NPOLN = 1
C                                       Check for blown arrays
         IF (NPOLN.GT.MAXPOL) THEN
            MSGTXT = 'NXTAVG: TOO MANY STOKES'
            IERR = 5
            GO TO 990
            END IF
C                                       Set axis arrays
         CALL FILL (5, 1, D)
         CALL FILL (10, 0, LIMS)
         DO 10 LOOP = 1,NDIM
            D(LOOP) = NAXIS(LOOP)
            LIMS(1,LOOP) = 1
            LIMS(2,LOOP) = NAXIS(LOOP)
 10         CONTINUE
C                                       Find the interval
         IF (INTERV.LE.0.0) THEN
            I4 = 18000
            CALL RFILL (I4, 0.0, BVIS)
            CALL UVREAD (NAME, RP, VS, IERR)
            IF (IERR.NE.0) GO TO 995
            I3 = 1
            STIME = RP(INDXT)
            SWT = 3600.0 * 24.0 * 10.0
            I2 = 0
C                                       read loop
 20         I3 = I3 + 1
            IF (I3.LT.10000) THEN
               CALL UVREAD (NAME, RP, VS, IERR)
               IF (IERR.GT.0) GO TO 995
               IF (IERR.LT.0) GO TO 30
               CTIME = RP(INDXT)
               I1 = (CTIME - STIME) * SWT + 0.5
               IF ((I1.GT.0) .AND. (I1.LT.I4)) THEN
                  I2 = I2 + 1
                  BVIS(I1) = BVIS(I1) + 1.0
                  END IF
               STIME = CTIME
               GO TO 20
               END IF
C                                       restart the IO
 30         CALL OUVCLO (NAME, IERR)
            IF (IERR.GT.0) GO TO 995
            CALL OUVOPN (NAME, 'READ', IERR)
            IF (IERR.NE.0) GO TO 995
C                                       find maxima
            STIME = 0.0
            CTIME = 0.0
            ADJI = 0.0
            BL2 = 0.0
            LSTIM = 0.0
            I1 = 0
            I3 = 0
            DO 35 I = 3,I4
               IF (BVIS(I).GT.CTIME) THEN
                  CTIME = BVIS(I)
                  I1 = I
                  END IF
 35            CONTINUE
            DO 40 I = 12,I4
               BLWT = (BVIS(I-1) + BVIS(I+1)) * 0.98 + BVIS(I)
               IF (BLWT.GT.STIME) THEN
                  LSTIM = BL2
                  BL2 = ADJI
                  ADJI = STIME
                  STIME = BLWT
                  I3 = I
                  END IF
 40            CONTINUE
            IF ((STIME.GT.1.5*LSTIM) .AND. (STIME.GE.CTIME)) THEN
               INTERV = I3 / SWT
            ELSE IF (ABS(I3-I1).LE.2) THEN
               INTERV = (2 * I3 + I1) / (3. * SWT)
            ELSE IF (I1.LT.12) THEN
               INTERV = I1 / SWT
            ELSE
               WRITE (MSGTXT,1035) I1, I3
               CALL MSGWRT (6)
               INTERV = I1 / SWT
               END IF
            BLWT = INTERV * SWT / 10.0
            WRITE (MSGTXT,1036) BLWT
            CALL MSGWRT (6)
            BLWT = BLWT / 60.0
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (NAME, 'SOLINT', OOARE, DIM, BLWT, CDUMMY, IERR)
            END IF
C                                       End of open section
         IERR = 0
         END IF
C                                       re-check interval
      IF (INTERV.LE.0.0) THEN
         INTERV = 10.0 / (24. * 3600.)
         MSGTXT = 'NXTAVG: AVERAGE INTERVAL SET TO 10 SECONDS'
         CALL MSGWRT (6)
         END IF
C                                       Initialize
      LSTIM = 0.0
      TIMSUM = 0.0D0
      TIMCNT = 0
      TIMMAX = -1.0E20
      TIMMIN = 1.0E20
C                                       Init output
      I1 = 2 * MAXPOL * MAXIFS * MAXAN * MAXAN
      CALL RFILL (I1, 0.0, VIS)
C                                       Extra antenna weights
      DO 90 LOOP = 1,MAXAN
         IF (ANTWT(LOOP).GT.0.0) THEN
            AWT(LOOP) = ANTWT(LOOP)
         ELSE
            AWT(LOOP) = 1.0
            END IF
 90      CONTINUE
C                                       Baseline weights, UVR in
C                                       wavelengths **2
      UVR(1) = UVRANG(1) * UVRANG(1) * 1.0E6
      UVR(2) = UVRANG(2) * UVRANG(2) * 1.0E6
      IF (UVR(2).LE.0.0) UVR(2) = 1.0E25
C                                       Loop reading, averaging data
 100  IF (READVS) CALL UVREAD (NAME, RP, VS, IERR)
         READVS = .FALSE.
         IF (IERR.GT.0) GO TO 995
C                                       Done?
         IF (IERR.LT.0) GO TO 500
C                                       Find time, source, etc
         CTIME = RP(INDXT)
         IF (INDXS.GE.0) THEN
            CSID = RP(INDXS) + 0.5
         ELSE
            CSID = -1
            END IF
         IF (INDXFQ.GE.0) THEN
            CFQID = RP(INDXFQ) + 0.5
         ELSE
            CFQID = -1
            END IF
C                                       Crack baseline
         IF (INDXB.GT.0) THEN
            ANT1 = (RP(INDXB) / 256.0) + 0.001
            ANT2 = (RP(INDXB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(INDXA1) + 0.1
            ANT2 = RP(INDXA2) + 0.1
            END IF
         IF ((SINGLE) .AND. (ANT1.LE.0)) ANT1 = ANT2
C                                       Ignore antennas with too large
C                                       values.
         IF ((ANT1.LE.0) .OR. (ANT1.GT.MAXAN). OR. (ANT2.LE.0) .OR.
     *      (ANT2.GT.MAXAN)) THEN
            READVS = .TRUE.
            GO TO 100
            END IF
C                                       Is this visibility in the
C                                       current average?
         IF (TIMCNT.LE.0) THEN
C                                       First vis in average - always
C                                       take it
            STIME = CTIME
C                                       Adjust end time.
            ADJI = MIN (0.1*INTERV, 1.157407E-5)
            LSTIM = CTIME + INTERV - ADJI
            SID = CSID
            FQID = CFQID
         ELSE
C                                       Check for consistency
            IF ((CTIME.GE.LSTIM) .OR. (SID.NE.CSID) .OR.
     *         (FQID.NE.CFQID)) GO TO 500
            END IF
C                                       Swallow this vis.
         READVS = .TRUE.
C                                       Time info
         TIMCNT = TIMCNT + 1
         TIMSUM = TIMSUM + RP(INDXT)
         TIMMAX = MAX (TIMMAX, RP(INDXT))
         TIMMIN = MIN (TIMMIN, RP(INDXT))
C                                       Baseline weight
         IF (SINGLE) THEN
            BL2 = 0.0
         ELSE
            BL2 = RP(INDXU)*RP(INDXU) + RP(INDXV)*RP(INDXV)
            END IF
         BLWT = 1.0
         IF ((BL2.LT.UVR(1)) .OR. (BL2.GT.UVR(2))) BLWT = WTUV
C                                       Average vis
         DO 400 I4 = 1,NPOLN
C                                       average in Poln?
            IF (.NOT.AVGPOL) THEN
               LIMS(1,INDXP) = I4
               LIMS(2,INDXP) = I4
               END IF
            DO 300 I3 = 1,NIF
C                                       average in IF?
               IF (.NOT.AVGIF .AND. (INDXIF.GT.0)) THEN
                  LIMS(1,INDXIF) = I3
                  LIMS(2,INDXIF) = I3
                  END IF
               CALL AVGVIS (D(2), D(3), D(4), D(5), LIMS, VS, VT)
C                                       Extra weights.
               VT(3) = VT(3) * AWT(ANT1) * AWT(ANT2) * BLWT
C                                       Update accumulation
               IF (VT(3).GT.0.0) THEN
                  VIS(1,I3,I4,ANT1,ANT2) = VIS(1,I3,I4,ANT1,ANT2) +
     *               VT(1) * VT(3)
                  VIS(1,I3,I4,ANT2,ANT1) = VIS(1,I3,I4,ANT2,ANT1) +
     *               VT(2) * VT(3)
                  VIS(2,I3,I4,ANT1,ANT2) = VIS(2,I3,I4,ANT1,ANT2) +
     *               VT(3)
                  VIS(2,I3,I4,ANT2,ANT1) = VIS(2,I3,I4,ANT2,ANT1) + 1.0
                  END IF
 300           CONTINUE
 400        CONTINUE
C                                       Loop until done
         GO TO 100
C                                       Normalize by sum of weights
 500  DO 540 I4 = 1,NPOLN
         DO 530 I3 = 1,NIF
            DO 520 I1 = 1,MAXAN-1
               DO 510 I2 = I1,MAXAN
                  SWT = VIS(2,I3,I4,I1,I2)
                  IF (SWT .GT. 1.0E-20) THEN
                     VIS(1,I3,I4,I1,I2) = VIS(1,I3,I4,I1,I2) / SWT
                     IF (I1.NE.I2) VIS(1,I3,I4,I2,I1) =
     *                  VIS(1,I3,I4,I2,I1)  /  SWT
                     END IF
 510              CONTINUE
 520           CONTINUE
 530        CONTINUE
 540     CONTINUE
C                                       Average time
      IF (TIMCNT.GT.0) THEN
         TIMEC = TIMSUM / TIMCNT
         TIMEI = TIMMAX - TIMMIN
      ELSE
         TIMEC = 0.0D0
         TIMEI = 0.0
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'NXTAVG:ERROR AVERAGING SCAN FOR ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1035 FORMAT ('Maxima in intervals not so clear: peaks at',2I5,
     *   ' 0.1 secs')
 1036 FORMAT ('NXTAVG: average interval set to',F8.2,' seconds')
      END
      SUBROUTINE UVCOPY (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies one uv data object to another with application of any
C   calibration editing, and selection criteria.
C   Can only process a single subarray at a time.  If multiple subarrays
C   are desired then make a separate call for each subarray with
C   'CALEDIT.SUBARR'  set on UVIN and 'UV_DESC.VISOFF' (zero relative
C   visibility offset) on UVOUT set to the current size of the object
C   ('UV_DESC.GCOUNT').
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  May be UVIN iff there
C                   is only one subarray.
C   Inputs attached to UVIN
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), ANVER, NSUBA, ISUBA, COUNT, ISUB1, ISUB2
      CHARACTER ANTAB*32, CDUMMY*1
      REAL      TIMEM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      CALL OUVGET (UVIN, 'CALEDIT.SUBARR', TYPE, DIM, ISUBA, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of subarrays
      ANTAB = 'Temp AN for SCLOOP'
      ANVER = 1
      CALL UV2TAB (UVIN, ANTAB, 'AN', ANVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, IERR)
      IF (IERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((ISUBA.GT.0) .AND. (ISUBA.LE.NSUBA)) THEN
         ISUB1 = ISUBA
         ISUB2 = ISUBA
      ELSE
         ISUB1 = 1
         ISUB2 = NSUBA
         END IF
      IF ((ISUB2.GT.ISUB1) .AND. (UVIN.EQ.UVOUT)) THEN
         MSGTXT = 'UVCOPY: CANNOT COPY > 1 SUBARRAY IN PLACE'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 990
         END IF
C                                       Start at beginning of output
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVOUT, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy
      TIMEM = -1.E8
      DO 50 ISUBA = ISUB1,ISUB2
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVCOP1 (UVIN, UVOUT, TIMEM, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set vis offset to append
         CALL OGET (UVOUT, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVOUT, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 50      CONTINUE
C                                       Reset selection on objects
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVOUT, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      IF (ISUB1.NE.ISUB2) THEN
         ISUBA = 0
         CALL OUVPUT (UVIN, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVPUT (UVOUT, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVCOPY: ERROR COPYING UVDATA'
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE UVCOP1 (UVIN, UVOUT, TIMEM, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies one uv data object to another with application of any
C   calibration editing, and selection criteria.
C   Can only process a single subarray at a time.  If multiple subarrays
C   are desired then make a separate call for each subarray with
C   'CALEDIT.SUBARR'  set on UVIN and 'UV_DESC.VISOFF' (zero relative
C   visibility offset) on UVOUT set to the current size of the object
C   ('UV_DESC.GCOUNT').
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  May be UVIN iff there
C                   is only one subarray.
C   Inputs attached to UVIN
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C   In/Out:
C      TIMEM   R    Maximum time in data set so far (used to turn off
C                   sort to time order).  < -1000. => FIRST CALL
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      REAL      TIMEM
      INTEGER   IERR
C
      INTEGER   COUNT, TYPE, DIM(7), MSGSAV, INDXU, INDXV, DUMMY,
     *   INDXT
      REAL      UMAX, VMAX
      CHARACTER SORD*2, TOUT*32, CDUMMY*1, UVTYPE*2
      LOGICAL   EXIST, WANT, SINGLE, FIRST, WRONG
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      FIRST = TIMEM.LT.-1.E3
      WRONG = .FALSE.
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      SINGLE = UVTYPE(1:1).EQ.'S'
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       U,V limits
      MSGSAV = MSGSUP
      IF (.NOT.SINGLE) THEN
         MSGSUP = 32000
         CALL OUVGET (UVIN, 'UMAX', TYPE, DIM, UMAX, CDUMMY, IERR)
         MSGSUP = MSGSAV
C                                       Default = all
         IF (IERR.EQ.1) THEN
            UMAX = 1.0E30
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 995
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL OUVGET (UVIN, 'VMAX', TYPE, DIM, VMAX, CDUMMY, IERR)
         MSGSUP = MSGSAV
C                                       Default = all
         IF (IERR.EQ.1) THEN
            VMAX = 1.0E30
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Temporary output in case in =
C                                       out.
      TOUT = 'Temporary output for UVCOP1'
      CALL OUVCOP (UVOUT, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (TOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Uv data pointers
      CALL UVDFND (UVIN, 1, 'TIME', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER TIME'
         GO TO 995
         END IF
      IF (.NOT.SINGLE) THEN
         CALL UVDFND (UVIN, 1, 'UU-L', INDXU, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
            GO TO 995
            END IF
         CALL UVDFND (UVIN, 1, 'VV-L', INDXV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
            GO TO 995
            END IF
         END IF
C                                       Initialize visibility count
      COUNT = 0
      IF (.NOT.FIRST) THEN
         CALL UVDGET (TOUT, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Want this one?
         IF (SINGLE) THEN
            WANT = .TRUE.
         ELSE
            WANT = (ABS (RP(INDXU)).LE.UMAX) .AND.
     *         (ABS (RP(INDXV)).LE.VMAX)
            END IF
         IF (WANT) THEN
            WRONG = RP(INDXT).LT.TIMEM
            TIMEM = MAX (TIMEM, RP(INDXT))
            COUNT = COUNT + 1
            CALL UVWRIT (TOUT, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVCOP1: NO DATA SELECTED'
         GO TO 995
         END IF
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (TOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input if
C                                       subarray 1, else undefined
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SORD(1:1).EQ.'T') THEN
         IF (WRONG) SORD = ' '
      ELSE
         IF (.NOT.FIRST) SORD = '  '
         END IF
      CALL UVDPUT (TOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy relevant tables
      IF (FIRST) THEN
         CALL UVDTCO (UVIN, TOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary object
      CALL OUVDES (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVCOP1: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UV2SCR (UVDATA, UVSCR, CMPSCR, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies A uv data file to a scratch file with application of any
C   calibration, editing, and selection criteria.  The vlbi '*V' data
C   weighting option can be selected and the u's and v's rotated.
C   Can only process a single subarray at a time.  If multiple subarrays
C   are desired then make a separate call for each subarray with
C   'CALEDIT.SUBARR'  set on UVDATA and 'UV_DESC.VISOFF' (zero relative
C   visibility offset) on UVSCR set to the current size of the object
C   ('UV_DESC.GCOUNT').
C
C   The UVSCR object will be created if it does not already exist; if
C   so, the disk-resident data willbe compressed if NCORR > 1 and
C   CMPSCR is true.
C
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      UVSCR   C*?  Name of scratch uvdata object.
C      CMPSCR  L    Compress scratch data on disk?
C   Inputs from UVDATA
C      UVWTFN  C*2  Data weighting option, if '*V' the weights are
C                   raised to the 0.25 power in the output object.
C                   Defaults to 'NA' (no weighting) if absent.
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*)
      LOGICAL   CMPSCR
      INTEGER   IERR
C
      INTEGER   NVIS, COUNT, TYPE, DIM(7), MSGSAV, NCORR, I, SUBA, K,
     *   INDXU, INDXV, DUMMY, NAXIS(7), NSTOK, JLOCS, TCOUNT, JLOCD,
     *   IBCH, IECH, IBIF, IEIF
      INTEGER   BSELET(51), ASELET(51), ACODE, BCODE, INDXB, ABSEL,
     *   INDXA1, INDXA2
      CHARACTER ISTOKE*4
      REAL      UMAX, VMAX, AUMAX, BUMAX, AVMAX, BVMAX, GUARDB(2),
     *   UTFACT(2), UU, VV, CELLS(2), CROT, SROT, ROTATE, CROTAU(7)
      DOUBLE PRECISION FREQS(1024), UVFREQ, FRMULT
      LOGICAL   DOVLWT, EXIST, DOWARN, DOROT, SINGLE
      CHARACTER UVWTFN*2, SORD*2, CDUMMY*1, CNAME*8, KEYW*8, UVTYPE*2
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input for descriptive info.
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      SINGLE = UVTYPE(1:1).EQ.'S'
C                                       UV WARNING
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'DOWARNING', TYPE, DIM, DOWARN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         DOWARN = .TRUE.
         END IF
C                                       VLBI weighting option?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'UVWTFN', TYPE, DIM, DUMMY, UVWTFN, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         UVWTFN = 'NA'
         END IF
      DOVLWT = UVWTFN(2:2) .EQ. 'V'
      IF (DOVLWT) THEN
         MSGTXT = 'UV2SCR: Applying VLBI weighting option'
         CALL MSGWRT (3)
         END IF
C                                       U,V limits
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'UMAX', TYPE, DIM, UMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         DOWARN = .FALSE.
         UMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'VMAX', TYPE, DIM, VMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         DOWARN = .FALSE.
         VMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Actual guardband
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         GUARDB(1) = 0.0
         GUARDB(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Default guardband
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'GUARDEF', TYPE, DIM, UTFACT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UTFACT(1) = 0.0
         UTFACT(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Intended cellsize
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CELLS(1) = 1.0
         CELLS(2) = 1.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Additional rotation
      IF (SINGLE) THEN
         ROTATE = 0.0
      ELSE
         MSGSUP = 32000
         CALL OUVGET (UVDATA, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            ROTATE = 0.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
      DOROT = ROTATE.NE.0.0
C                                       CROTA
      IF (DOROT) THEN
         CALL UVDGET (UVDATA, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVDFND (UVDATA, 2, 'DEC', JLOCD, IERR)
         IF (IERR.NE.0) GO TO 990
         CROTAU(JLOCD) = CROTAU(JLOCD) + ROTATE
         ROTATE = -ROTATE / 57.29578
         CROT = COS (ROTATE)
         SROT = SIN (ROTATE)
         END IF
C                                       Find number of correlations
      CALL OUVGET (UVDATA, 'UV_DESC.NCORR', TYPE, DIM, NCORR, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UV axis descriptor
C                                       NAXIS
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 2, 'STOKES', JLOCS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of polarizations
      NSTOK = NAXIS(JLOCS)
C                                       Freq array
      CALL UVFRQS (UVDATA, UVFREQ, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average frequency
      K = 0
      FRMULT = 0.0D0
      DO 30 I = 1,NCORR,NSTOK
         K = K + 1
         FRMULT = MAX (FRMULT, FREQS(K))
 30      CONTINUE
      FRMULT = FRMULT / UVFREQ
C                                       Uv data pointers
      IF (.NOT.SINGLE) THEN
         CALL UVDFND (UVDATA, 1, 'UU-L', INDXU, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
            GO TO 995
            END IF
         CALL UVDFND (UVDATA, 1, 'VV-L', INDXV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
            GO TO 995
            END IF
C                                       check if baseline selection is on
C                                       OOP-baseline selection is
C                                       implemented as follows.
C                                       If only the array BSELECT is
C                                       specified, then a baseline AB will
C                                       pass through if either A or B is
C                                       in the array BSELECT.  ABSELECT
C                                       simply equals the number of
C                                       elements in the array BSELECT.
C
C                                       If both arrays BSELECT and ASELECT
C                                       are specified, then a baseline AB
C                                       will pass through if either
C                                       A is in ASELECT and B is in BSELECT
C                                       or
C                                       B is in ASELECT and A is in BSELECT.
C                                       ABSELECT should equal NM where
C                                       N is the length of ASELECT and
C                                       M is the length of BSELECT
         MSGSUP = 32000
         CALL OGET (UVDATA, 'ABSELECT', TYPE, DIM, ABSEL, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.NE.0) THEN
            IERR = 0
            ABSEL = 0
            END IF
C                                       baseline selection is on
         IF (ABSEL.GT.0) THEN
            MSGTXT = 'Using OOP-based baseline selection'
            CALL MSGWRT (2)
C                                       only one baseline end is specified
            IF (ABSEL.LT.256) THEN
               CALL OGET (UVDATA, 'BSELECT', TYPE, DIM, ASELET, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'Problem activating OOP-baseline selection'
                  GO TO 995
                  END IF
C                                       this is the number of elements on
C                                       the ASELET list
               ASELET(51) = ABSEL
               ABSEL = 1
C                                       both baseline ends are specified
            ELSE
               CALL OGET (UVDATA, 'ASELECT', TYPE, DIM, ASELET, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'Problem activating OOP-baseline selection'
                  GO TO 995
                  END IF
               ASELET(51) = ABSEL/256 + 0.1
               ABSEL = ABSEL - ASELET(51)*256 + 0.1
               IF (ABSEL.GT.0) THEN
                  CALL OGET (UVDATA, 'BSELECT', TYPE, DIM, BSELET,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'Problem activating OOP-baseline'
     *                  // ' selection'
                     GO TO 995
                     END IF
C                                       this is the number of elements
C                                       on the BSELET list
                  BSELET(51) = ABSEL
                  END IF
               ABSEL = 2
               END IF
            CALL UVDFND (UVDATA, 1, 'BASELINE', INDXB, IERR)
            IF (IERR.NE.0) THEN
               INDXB = -1
               CALL UVDFND (UVDATA, 1, 'ANTENNA1', INDXA1, IERR)
               IF (IERR.EQ.0) CALL UVDFND (UVDATA, 1, 'ANTENNA2',
     *            INDXA2, IERR)
               END IF
            IF (IERR.NE.0) THEN
               MSGTXT = 'Problem: UV file has no BASELINE random' //
     *            ' parameter'
               GO TO 995
               END IF
            END IF
         END IF
C                                       Create output if necessary
      CALL OBFEXS (UVSCR, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
C                                       Move selection criteria
C                                       since it has already been
C                                       applied
         CALL SECSLT (UVDATA, IBIF, IEIF, IBCH, IECH, ISTOKE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL SECSAV (UVDATA, 0, 0, 0, 0, ' ', IERR)
         IF (IERR.NE.0) GO TO 990
         NVIS = 1000
         CALL OUVSCR (UVSCR, UVDATA, NVIS, CMPSCR, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL SECSAV (UVDATA, IBIF, IEIF, IBCH, IECH, ISTOKE, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy data
      CALL OUVOPN (UVSCR, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVDATA, UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Subarray number
      CALL SECGET (UVDATA, 'SUBARR', TYPE, DIM, SUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBA.LE.0) SUBA = 1
C                                       Declare 'MAXBLINE' a header
C                                       keyword for the uvdata class.
      BUMAX = 0.0
      IF (.NOT.SINGLE) THEN
         CNAME = 'UVDATA'
         KEYW = 'MAXBLINE'
         CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get current max u
         MSGSUP = 32000
         CALL OUVGET (UVSCR, 'MAXBLINE', TYPE, DIM, BUMAX, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            BUMAX = 0.0
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Initialize visibility count
      COUNT = 0
      TCOUNT = 0
      AUMAX = 0.0
      AVMAX = 0.0
      BVMAX = 0.0
      UMAX = UMAX / FRMULT
      VMAX = VMAX / FRMULT
      UU = 0.0
      VV = 0.0
      IF (SUBA.GT.1) THEN
         CALL UVDGET (UVSCR, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Loop thru data
 100     CALL UVREAD (UVDATA, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Want this one?
         IF (.NOT.SINGLE) THEN
C                                       If ABSEL=0, no ASELET, BSELET
C                                       selection
            IF (ABSEL.GT.0) THEN
C                                       select baselines here:
C                                       get current baseline code
C                                       get Astation and Bstation codes
               IF (INDXB.GT.0) THEN
                  BCODE = RP(INDXB) + 0.1
                  ACODE = BCODE/256
                  BCODE = BCODE - ACODE*256
               ELSE
                  ACODE = RP(INDXA1) + 0.1
                  BCODE = RP(INDXA2) + 0.1
                  END IF
C                                       if only one baseline end was selected
               IF (ABSEL.EQ.1) THEN
C                                       check if either end of this baseline
C                                       matches Astation or Bstation
                  DO 40 I = 1, ASELET(51)
                     IF (ACODE.EQ.ASELET(I)) GO TO 60
                     IF (BCODE.EQ.ASELET(I)) GO TO 60
 40                  CONTINUE
C                                       otherwise both ends were selected
               ELSE
                  DO 50 I = 1,ASELET(51)
                     DO 45 K = 1,BSELET(51)
C                                       check if both ends of this baseline
C                                       match the Astation and Bstation codes
                        IF ((ACODE.EQ.ASELET(I)).AND.
     *                     (BCODE.EQ.BSELET(I))     ) GO TO 60
                        IF ((BCODE.EQ.ASELET(I)).AND.
     *                     (ACODE.EQ.BSELET(I))     ) GO TO 60
 45                     CONTINUE
 50                  CONTINUE
                  END IF
               GO TO 100
C                                       jump to here if baseline selection
C                                       matches
 60            CONTINUE
C                                       or was turned off
               END IF
            IF (DOROT) THEN
               UU = RP(INDXU)
               VV = RP(INDXV)
               RP(INDXU) = CROT * UU - SROT * VV
               RP(INDXV) = CROT * VV + SROT * UU
               END IF
            UU = ABS (RP(INDXU))
            VV = ABS (RP(INDXV))
            AUMAX = MAX (UU, AUMAX)
            AVMAX = MAX (VV, AVMAX)
            END IF
         TCOUNT = TCOUNT + 1
         IF ((UU.LE.UMAX) .AND. (VV.LE.VMAX)) THEN
            BUMAX = MAX (UU, BUMAX)
            BVMAX = MAX (VV, BVMAX)
            COUNT = COUNT + 1
C                                       Change weighting?
            IF (DOVLWT) THEN
               DO 150 I = 1,NCORR
                  IF (VS(3,I).GT.0.0) VS(3,I) = VS(3,I) ** 0.25
 150              CONTINUE
               END IF
            CALL UVWRIT (UVSCR, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
C                                       Set amount of output data
      IF (.NOT.SINGLE) THEN
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVSCR, 'MAXBLINE', OOARE, DIM, BUMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       rotation
      IF (DOROT) THEN
         ROTATE = 0.0
         CALL OUVPUT (UVSCR, 'ROTATE', OOARE, DIM, ROTATE, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 7
         CALL UVDPUT (UVSCR, 'CROTA', OOARE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Warnings ?
      IF (DOWARN) THEN
         IF (TCOUNT.GT.COUNT) THEN
            WRITE (MSGTXT,1200) COUNT, TCOUNT
            CALL MSGWRT (6)
         ELSE
            WRITE (MSGTXT,1201) COUNT
            CALL MSGWRT (3)
            END IF
C                                       data outside grid?
         UMAX = UMAX * FRMULT
         VMAX = VMAX * FRMULT
         AUMAX = AUMAX * FRMULT
         AVMAX = AVMAX * FRMULT
         BUMAX = BUMAX * FRMULT
         BVMAX = BVMAX * FRMULT
         IF ((AUMAX.GT.UMAX) .OR. (AVMAX.GT.VMAX)) THEN
            MSGTXT = '**** WARNING data discarded outside usable part'
     *         // ' of UV plane ****'
            CALL MSGWRT (6)
            IF (AUMAX.GT.UMAX) THEN
               UU = UMAX/AUMAX * ABS(CELLS(1))
               WRITE (MSGTXT,1210) 'U', AUMAX, UMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1211) UU, ABS(CELLS(1))
               CALL MSGWRT (6)
               WRITE (MSGTXT,1212) 'U', GUARDB(1)
               CALL MSGWRT (6)
               END IF
            IF (AVMAX.GT.VMAX) THEN
               VV = VMAX/AVMAX * ABS(CELLS(2))
               WRITE (MSGTXT,1210) 'V', AVMAX, VMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1211) VV, CELLS(2)
               CALL MSGWRT (6)
               WRITE (MSGTXT,1212) 'V', GUARDB(2)
               CALL MSGWRT (6)
               END IF
            END IF
         IF ((BUMAX.GT.UMAX*(1.-1.2*UTFACT(1))/(1.0-GUARDB(1))) .OR.
     *      (BVMAX.GT.VMAX*(1.-1.2*UTFACT(2))/(1.0-GUARDB(2)))) THEN
            MSGTXT = '**** WARNING data included out of inner portion'
     *         // ' of UV plane ****'
            CALL MSGWRT (6)
            MSGTXT = '**** Watch for high-frequency & other poor ' //
     *         'cleaning effects ****'
            CALL MSGWRT (6)
            END IF
C                                       set DOWARN away
         DOWARN = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVDATA, 'DOWARNING', OOALOG, DIM, DOWARN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UV2SCR: NO DATA SELECTED'
         GO TO 995
         END IF
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (UVSCR, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy relevant tables
      MSGSUP = 31999
      CALL UVDTCO (UVDATA, UVSCR, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVDATA, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBA.GT.1) SORD = '  '
      IF ((DOROT) .AND. (SORD.NE.'TB') .AND. (SORD.NE.'BT')) SORD = '??'
      CALL UVDPUT (UVSCR, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UV2SCR: ERROR COPYING ' // UVDATA
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVSCR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('UV2SCR: Only',I9,' samples of',I9,' fell on UV grid')
 1201 FORMAT ('UV2SCR: Copied',I9,' visibilities to be imaged')
 1210 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1211 FORMAT ('**** Use cellsize <',F10.5,' not',F10.5,
     *   ' to get all data ****')
 1212 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
      END
      SUBROUTINE UVRSCR (UVDATA, UVSCR, CMPSCR, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies A uv data file to a scratch file with no calibration and no
C   data selection of any sort.  Tables are not copied.  The UVSCR
C   object will be created if it does not already exist; if so, the
C   disk-resident data will be compressed if NCORR > 1 and  CMPSCR is
C   true.C
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      UVSCR   C*?  Name of scratch uvdata object.
C      CMPSCR  L    Compress scratch data on disk?
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*)
      LOGICAL   CMPSCR
      INTEGER   IERR
C
      INTEGER   NVIS, COUNT, DIM(7), IBCH, IECH, IBIF, IEIF, ICOUNT,
     *   TYPE, MSGSAV
      CHARACTER ISTOKE*4, CDUMMY*1
      LOGICAL   EXIST
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Open input for descriptive info.
      CALL OUVOPN (UVDATA, 'RRAW', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVSCR, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Move selection criteria
C                                       since it has already been
C                                       applied
      IF (.NOT.EXIST) THEN
         CALL SECSLT (UVDATA, IBIF, IEIF, IBCH, IECH, ISTOKE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL SECSAV (UVDATA, 0, 0, 0, 0, ' ', IERR)
         IF (IERR.NE.0) GO TO 990
         NVIS = 1000
         CALL OUVSCR (UVSCR, UVDATA, NVIS, CMPSCR, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL SECSAV (UVDATA, IBIF, IEIF, IBCH, IECH, ISTOKE, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy data
      CALL OUVOPN (UVSCR, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVDATA, UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize visibility count
      CALL UVDGET (UVDATA, 'GCOUNT', TYPE, DIM, ICOUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       Loop thru data
 100     CALL UVREAD (UVDATA, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
         COUNT = COUNT + 1
         CALL UVWRIT (UVSCR, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
         GO TO 100
C
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVRSCR: NO DATA SELECTED'
         GO TO 995
      ELSE IF (COUNT.NE.ICOUNT) THEN
         WRITE (MSGTXT,1200) COUNT, ICOUNT
         CALL MSGWRT (7)
         END IF
C                                       Copy relevant tables
      MSGSUP = 31999
      CALL UVDTCO (UVDATA, UVSCR, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 990
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (UVSCR, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVRSCR: ERROR COPYING ' // UVDATA
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVSCR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('UVRSCR: COPIED',I10,' OF',I10,' RECORDS - SOMETHING ??')
      END
      SUBROUTINE UVCLIP (UVIN, UVOUT, NFLAG, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies one uv data object to another flagging any specified Stokes'
C   correlations whose amplitude exceeds MAXAMP.
C   If either I polarization is flagged then both cross polarized
C   correlations (if present) are flagged.
C      NOTE: no tables are copied.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  Will be instantiated
C                   if necessary.  May NOT be same object as UVIN
C                   although the underlying files may be.
C   Inputs attached to UVIN:
C      MAXAMP    R    Maximum amplitude.
C      CLPSTOKE  C*4  Stokes type to clip (default 'I')
C                     Recognizes 'I', 'Q', 'U', 'V'.
C                     On 'I' all correlations are flagged with 'I'.
C   Output:
C      NFLAG   I    Number of visibilities flagged
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   NFLAG, IERR
C
      REAL      MAXAMP
      CHARACTER CSTOKE*4
C
      INTEGER   COUNT, TYPE, DIM(7), NDIM, NAXIS(7), IILOCU, IILOCV,
     *   IILOCW, IILOCT, IILOCB, IILCSU, IILCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JJLOCC, JJLOCS, JJLOCF, JJLOCR, JJLOCD, JJLCIF, IINCS, IINCF,
     *   IICIF, IIF, ICHAN, IPOLN, NIF, NCHAN, NPOLN, NPPOL, INDEX,
     *   NOFLAG, MSGSAV, OFF1, OFF2, ICOR, INDXS1, INDXS2, DUMMY
      DOUBLE PRECISION CRVAL(7)
      CHARACTER SORD*2, CDUMMY*1
      REAL      MAX2, AMP2, FACT1, FACT2, VVV(2)
      LOGICAL   EXIST, DOXPOL, FLAG, OK, TRUSTO, ISIPOL
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get clipping value
      CALL OUVGET (UVIN, 'MAXAMP', TYPE, DIM, MAXAMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MAX2 = MAXAMP * MAXAMP
C                                       Optional Stokes' for clipping
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'CLPSTOKE', TYPE, DIM, DUMMY, CSTOKE, IERR)
      MSGSUP = MSGSAV
C                                       Default = 'I'
      IF (IERR.EQ.1) THEN
         CSTOKE = 'I'
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get vis info
      CALL UVDGET (UVIN, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (UVIN, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDPNT (UVIN, IILOCU, IILOCV, IILOCW, IILOCT, IILOCB, IILCSU,
     *   IILCFQ, ILOCA1, ILOCA2, ILOCSA, JJLOCC, JJLOCS, JJLOCF, JJLOCR,
     *   JJLOCD, JJLCIF, IINCS, IINCF, IICIF, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Convert increments to work with
C                                       VS.
      IINCS = IINCS / 3
      IINCF = IINCF / 3
      IICIF = IICIF / 3
C                                       Dimension of axes
C                                       Frequency
      NCHAN = NAXIS(JJLOCF)
C                                       IF (may not exist)
      IF (JJLCIF.GT.0) THEN
         NIF = NAXIS(JJLCIF)
      ELSE
         NIF = 1
         END IF
C                                       Stokes'
      NPOLN = NAXIS(JJLOCS)
      NPPOL = MIN (2, NPOLN)
C                                       IQUV?
      TRUSTO = CRVAL(JJLOCS).GT.0.0D0
      IF (TRUSTO) NPPOL = 1
C                                       Flag cross polarizations?
      DOXPOL = (NPOLN .GT. NPPOL) .AND. (CSTOKE.EQ.'I')
C                                       Are we doing IPOL
      ISIPOL = CSTOKE.EQ.'I'
C                                       First correlation
      IF (CRVAL(JJLOCS).GT.0.0) THEN
         ICOR = CRVAL(JJLOCS) + 0.5D0
      ELSE
         ICOR = CRVAL(JJLOCS) - 0.5D0
         END IF
C                                       Pointers and factors for Stokes'
C                                       conversion.
      OFF2 = 0
      FACT2 = 0.0
      IF (CSTOKE.EQ.'I') THEN
         IF (TRUSTO) THEN
            OFF1 = 1 - ICOR
            OFF2 = OFF1
            FACT1 = 1.0
         ELSE
            OFF1 = 0
            FACT1 = 0.5
            IF (NPOLN.GT.1) THEN
               OFF2 = 1
               FACT2 = 0.5
               END IF
            END IF
      ELSE IF (CSTOKE.EQ.'Q') THEN
         IF (TRUSTO) THEN
            OFF1 = 2 - ICOR
            FACT1 = 1.0
            OFF2 = OFF1
         ELSE
            OFF1 = 2
            FACT1 = 0.5
            OFF2 = 3
            FACT2 = 0.5
            END IF
      ELSE IF (CSTOKE.EQ.'U') THEN
         IF (TRUSTO) THEN
            OFF1 = 3 - ICOR
            FACT1 = 1.0
            OFF2 = OFF1
         ELSE
            OFF1 = 2
            FACT1 = -0.5
            OFF2 = 3
            FACT2 = 0.5
            END IF
      ELSE IF (CSTOKE.EQ.'V') THEN
         IF (TRUSTO) THEN
            OFF1 = 4 - ICOR
            FACT1 = 1.0
            OFF2 = OFF1
         ELSE
            OFF1 = 0
            FACT1 = 0.5
            OFF2 = 1
            FACT2 = -0.5
            END IF
      ELSE
C                                       Unrecognized Stokes'
         IERR = 1
         MSGTXT = 'UNRECOGNIZED STOKES FOR CLIPPING: ' // CSTOKE
         GO TO 995
         END IF
C                                       Is data available?
      IF ((OFF1+1.GT.NPOLN) .OR. (OFF2+1.GT.NPOLN) .OR. (OFF1.LT.0) .OR.
     *   (OFF2.LT.0) ) THEN
            MSGTXT = 'INSUFFICIENT DATA FOR REQUESTED STOKES ' // CSTOKE
            IERR = 2
            GO TO 995
         END IF
C                                       Initialize visibility count
      COUNT = 0
      NOFLAG = 0
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Loop thru correlations
         OK = .FALSE.
         DO 140 IIF = 1,NIF
            DO 130 ICHAN = 1,NCHAN
               FLAG = .FALSE.
               INDEX = 1 + (IIF-1) * IICIF + (ICHAN-1) * IINCF
               INDXS1 = INDEX + OFF1 * IINCS
               INDXS2 = INDEX + OFF2 * IINCS
               IF ((VS(3,INDXS1).GT.0.0) .AND.
     *            (VS(3,INDXS2).GT.0.0))  THEN
C                                       Get requested visibility
                  VVV(1) = FACT1 * VS(1,INDXS1) + FACT2 * VS(1,INDXS2)
                  VVV(2) = FACT1 * VS(2,INDXS1) + FACT2 * VS(2,INDXS2)
C                                       No need for U times i
C                                       correction.
C                                       Case of IPOL and only one of
C                                       pair.
               ELSE IF (ISIPOL) THEN
                  IF (VS(3,INDXS1).GT.0.0) THEN
                     VVV(1) = VS(1,INDXS1)
                     VVV(2) = VS(2,INDXS1)
                  ELSE IF (VS(3,INDXS2).GT.0.0) THEN
                     VVV(1) = VS(1,INDXS2)
                     VVV(2) = VS(2,INDXS2)
                  ELSE
                     VVV(1) = 0.0
                     VVV(2) = 0.0
                     END IF
               ELSE
                  VVV(1) = 0.0
                  VVV(2) = 0.0
                  END IF
                  AMP2 = VVV(1) * VVV(1) + VVV(2) * VVV(2)
                  IF (AMP2.GT.MAX2) THEN
                     VS(3,INDXS1) = -ABS (VS(3,INDXS1))
                     VS(3,INDXS2) = -ABS (VS(3,INDXS2))
                     FLAG = .TRUE.
                     NOFLAG = NOFLAG + 1
                  ELSE
C                                       Count good ones
                     COUNT = COUNT + 1
                     OK = .TRUE.
                     END IF
               IF (DOXPOL.AND.FLAG) THEN
C                                       Flag all cross poln.
                  DO 120 IPOLN = NPPOL+1,NPOLN
                     VS(3,INDEX) = -ABS (VS(3,INDEX))
                     INDEX = INDEX + IINCS
 120                 CONTINUE
                  END IF
 130           CONTINUE
 140        CONTINUE
         IF (OK) THEN
            CALL UVWRIT (UVOUT, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVCLIP: NO DATA LEFT'
         GO TO 995
         END IF
C                                       Tell how many flagged
      IF (NOFLAG.GT.0) THEN
         WRITE (MSGTXT,1200) NOFLAG, MAXAMP
         CALL MSGWRT (4)
      ELSE
         MSGTXT = 'UVCLIP: No data flagged'
         CALL MSGWRT (4)
         END IF
C                                       Return number flagged
      NFLAG = NOFLAG
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVCLIP: ERROR CLIPPING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('UVCLIP: Flagged ',I7,' vis > ',1PE12.5)
      END
      SUBROUTINE UVVCLP (UVIN, UVOUT, NFLAG, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies one uv data object to another flagging based on excessive
C   VPOL.   All polarizations are flagged if the VPOL value is greater
C   than sqrt(MAXTRM(1)**2 + (MAXTRM(1)*IPOL)**2)).
C   ONLY works on RR,LL (LR, RL data).
C      NOTE: no tables are copied.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  Will be instantiated
C                   if necessary.  May NOT be same object as UVIN
C                   although the underlying files may be.
C   Inputs attached to UVIN:
C      MAXTRM  R(2) Maximum amplitude VPOL polarization terms.
C   Output:
C      NFLAG   I    Number of visibilities flagged
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   NFLAG, IERR
C
      INTEGER   COUNT, TYPE, DIM(7), NDIM, NAXIS(7), IILOCU, IILOCV,
     *   IILOCW, IILOCT, IILOCB, IILCSU, IILCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JJLOCC, JJLOCS, JJLOCF, JJLOCR, JJLOCD, JJLCIF, IINCS, IINCF,
     *   IICIF, IIF, ICHAN, IPOLN, NIF, NCHAN, NPOLN, INDEX, NOFLAG,
     *   OFFV2, DUMMY
      DOUBLE PRECISION CRVAL(7)
      CHARACTER SORD*2, CDUMMY*1
      REAL      MAXTRM(2), MAX2, VPOL2, IPOL2, MXT1, MXT2
      LOGICAL   EXIST, FLAG, OK, ISIQUV
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get clipping value
      CALL OUVGET (UVIN, 'MAXTRM', TYPE, DIM, MAXTRM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MXT1 = MAXTRM(1)**2
      MXT2 = MAXTRM(2)**2
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get vis info
      CALL UVDGET (UVIN, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDGET (UVIN, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL UVDPNT (UVIN, IILOCU, IILOCV, IILOCW, IILOCT, IILOCB, IILCSU,
     *   IILCFQ, ILOCA1, ILOCA2, ILOCSA, JJLOCC, JJLOCS, JJLOCF, JJLOCR,
     *   JJLOCD, JJLCIF, IINCS, IINCF, IICIF, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Convert increments to work with
C                                       VS.
      IINCS = IINCS / 3
      IINCF = IINCF / 3
      IICIF = IICIF / 3
C                                       Dimension of axes
C                                       Frequency
      NCHAN = NAXIS(JJLOCF)
C                                       IF (may not exist)
      IF (JJLCIF.GT.0) THEN
         NIF = NAXIS(JJLCIF)
      ELSE
         NIF = 1
         END IF
C                                       Stokes'
      NPOLN = NAXIS(JJLOCS)
C                                       IQUV?
      ISIQUV = CRVAL(JJLOCS) .GT. 0.0D0
C                                       Check that necessary date is
C                                       present.
      IF ((ISIQUV.AND.(NPOLN.LT.4)) .OR. (.NOT.ISIQUV.AND.(NPOLN.LT.2)))
     *   THEN
         IERR = 5
         MSGTXT = 'UVVCLP: DATA DOES NOT CONTAIN NECESSARY POLARIZATION'
         GO TO 995
         END IF
C                                       Initialize visibility count
      COUNT = 0
      NOFLAG = 0
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Offset for second poln.
         IF (ISIQUV) THEN
            OFFV2 = 3 * IINCS
         ELSE
            OFFV2 = IINCS
            END IF
C                                       Loop thru correlations
         OK = .FALSE.
         DO 140 IIF = 1,NIF
            DO 130 ICHAN = 1,NCHAN
               FLAG = .FALSE.
               INDEX = 1 + (IIF-1) * IICIF + (ICHAN-1) * IINCF
               IF ((VS(3,INDEX).GT.0.0).AND.(VS(3,INDEX+OFFV2).GT.0.0))
     *            THEN
C                                       Compute IPOL2 and VPOL2
                  IF (ISIQUV) THEN
C                                       IQUV
                     IPOL2 = (VS(1,INDEX)**2) + (VS(2,INDEX)**2)
                     VPOL2 = (VS(1,INDEX+OFFV2)**2) +
     *                  (VS(2,INDEX+OFFV2)**2)
                  ELSE
C                                       RR,LL etc
                     IPOL2 = 0.5 * (((VS(1,INDEX)+VS(1,INDEX+OFFV2))**2)
     *                  + ((VS(2,INDEX)+VS(2,INDEX+OFFV2))**2))
                     VPOL2 =0.5 * (((VS(1,INDEX)-VS(1,INDEX+OFFV2))**2)
     *                  + ((VS(2,INDEX)-VS(2,INDEX+OFFV2))**2))
                     END IF
C                                       Clip level
                  MAX2 = MXT1 + MXT2 * IPOL2
                  IF (VPOL2.GT.MAX2) THEN
                     FLAG = .TRUE.
                     NOFLAG = NOFLAG + 1
                  ELSE
C                                       Count good ones
                     COUNT = COUNT + 1
                     OK = .TRUE.
                     END IF
C                                       Not both RR and LL flag all
                  ELSE
                     FLAG = .TRUE.
                  END IF
                  IF (FLAG) THEN
C                                       Flag all  poln.
                     DO 120 IPOLN = 1,NPOLN
                        VS(3,INDEX) = -ABS (VS(3,INDEX))
                        INDEX = INDEX + IINCS
 120                    CONTINUE
                     END IF
 130           CONTINUE
 140        CONTINUE
         IF (OK) THEN
            CALL UVWRIT (UVOUT, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVVCLP: NO DATA LEFT'
         GO TO 995
         END IF
C                                       Tell how many flagged
      IF (NOFLAG.GT.0) THEN
         WRITE (MSGTXT,1200) NOFLAG, MAXTRM
         CALL MSGWRT (4)
      ELSE
         MSGTXT = 'UVVCLP: No data flagged'
         CALL MSGWRT (4)
         END IF
C                                       Return number flagged
      NFLAG = NOFLAG
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPUT (UVOUT, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVVCLP: ERROR VPOL CLIPPING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('UVVCLP: Flagged ',I7,' vis > ',1PE12.5, 1X, E12.5)
      END
      SUBROUTINE U2IDES (UVDATA, IMAGE, DODFT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies descriptive info from a Uvdata object to an image and
C   initializes the image descriptors.
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      IMAGE   C*?  Name of Image object.
C      DODFT   L    If .TRUE. using DFT imaging
C   Inputs from IMAGE object:
C      IMSIZE   I(2) Image size in pixels
C      CELLSIZE R(2) Cell size in arc seconds.
C      OPTYPE   C*4  'LINE' or 'SUM' (default 'SUM')
C      SHIFT    R(2) Shift in arcsec
C      ISBEAM   L    If .TRUE. then IMAGE is a BEAM
C   Inputs from UVDATA object: (defaults enforced).
C      BIF      I    First IF selected
C      EIF      I    Highest IF selected.
C      BCHAN    I    First channel selected
C      ECHAN    I    Highest channel selected.
C      CHINC    I    Channel increment
C      STOKES   C*4  Stokes selected
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), IMAGE*(*)
      LOGICAL   DODFT
      INTEGER   IERR
C
      INTEGER   NDESC
C                                       NDESC = number of descriptors to
C                                       copy.
      PARAMETER (NDESC = 7)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), NDIMU, NDIMI, NAXISU(7), NAXISI(7),
     *   LOOP, IVAL(50), DUMMY, IMSIZE(2,MAXFLD), BIF, EIF, BCHAN,
     *   ECHAN, CHINC, IMOBJ, UVOBJ, MSGSAV, VELREF, NCHAV, DEPTH(5),
     *   CATSAV(256), SDIWT, IILOCU, IILOCV, IILOCW, IILOCT, IILOCB,
     *   IILCSU, IILCFQ, IILCA1, IILCA2, IILCSA, JJLOCC, JJLOCS, JJLOCF,
     *   JJLOCR, JJLOCD, JJLCIF, IINCS, IINCF, IICIF
      LOGICAL   ISBEAM
      REAL      CDELTU(7), CDELTI(7), CRPIXU(7), CRPIXI(7), CROTAU(7),
     *   CROTAI(7), CELSIZ(2), SHIFT(2), ALTPIX, XSHFT, YSHFT, MAPROT,
     *   REFPIX, XPIX, YPIX, IMGROT
      DOUBLE PRECISION CRVALU(7), CRVALI(7), ALTVAL, RESTFQ, RA, DEC,
     *   DG2RAD, REFREQ, COORD(2), AVFREQ, OBSRA, OBSDEC
      CHARACTER CTYPEI(7)*8, CTYPS(7)*8, BUNIT*8, CVAL*100, CDUMMY*1,
     *   CDESC(NDESC)*8, CHTYPE*4, STOKES*4, UVTYPE*4, PROJ*4,
     *   CTYPEU(7)*8, PTYPEU(14)*8
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CDESC /'OBJECT', 'TELESCOP', 'INSTRUME', 'OBSERVER',
     *   'DATE-OBS', 'EPOCH', 'USERNO'/
      DATA CTYPS /'RA---SIN', 'DEC--SIN', 'FREQ', 'STOKES', 3*'    '/
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Open and close uvdata to fully
C                                       define object.
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDPNT (UVDATA, IILOCU, IILOCV, IILOCW, IILOCT, IILOCB,
     *   IILCSU, IILCFQ, IILCA1, IILCA2, IILCSA, JJLOCC, JJLOCS, JJLOCF,
     *   JJLOCR, JJLOCD, JJLCIF, IINCS, IINCF, IICIF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'CTYPE', TYPE, DIM, IVAL, CTYPEU, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'PTYPE', TYPE, DIM, IVAL, PTYPEU, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy descriptive stuff
      DO 20 LOOP = 1,NDESC
         CALL UVDGET (UVDATA, CDESC(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDPUT (IMAGE, CDESC(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
 20      CONTINUE
C                                       Initialize image axis and array
C                                       descriptors
      NDIMI = 4
      DO 50 LOOP = 1,7
         NAXISI(LOOP) = 0
         CDELTI(LOOP) = 0.0
         CRPIXI(LOOP) = 0.0
         CROTAI(LOOP) = 0.0
         CRVALI(LOOP) = 0.0D0
         CTYPEI(LOOP) = CTYPS(LOOP)
 50      CONTINUE
C                                       Type of uv data
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       projection
      PROJ = '-SIN'
      IF (UVTYPE.EQ.'UP') THEN
         PROJ = '-NCP'
         CTYPEI(1)(5:) = PROJ
         CTYPEI(2)(5:) = PROJ
         END IF
C                                       check coord types : SD only
      IF (UVTYPE.EQ.'SD') THEN
         IF ((CTYPEU(JJLOCR).NE.PTYPEU(IILOCU)) .OR.
     *      (CTYPEU(JJLOCD).NE.PTYPEU(IILOCV))) THEN
             MSGTXT = 'RANDOM PARAMETERS AND COORDINATES DO NOT MATCH'
             CALL MSGWRT (8)
             IERR = 10
             GO TO 990
             END IF
         CTYPEI(1) = CTYPEU(JJLOCR)
         CTYPEI(2) = CTYPEU(JJLOCD)
         IF (CTYPEI(1)(3:4).EQ.'  ') CTYPEI(1)(3:4) = '--'
         IF (CTYPEI(2)(4:4).EQ.' ') CTYPEI(2)(4:4) = '-'
         END IF
C                                       single dish
      IF ((UVTYPE.EQ.'SD') .OR. (UVTYPE.EQ.'SB')) THEN
         MSGSUP = 32000
         CALL OGET (IMAGE, 'SDTYPE', TYPE, DIM, SDIWT, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            SDIWT = -1
            END IF
         IF (IERR.NE.0) GO TO 990
         MSGSUP = 32000
         CALL OGET (IMAGE, 'CPROJ', TYPE, DIM, DUMMY, PROJ, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            PROJ = ' '
            END IF
         IF (IERR.NE.0) GO TO 990
         IF (PROJ(1:1).EQ.'-') THEN
            CTYPEI(1)(5:8) = PROJ
            CTYPEI(2)(5:8) = PROJ
         ELSE
            PROJ = '-SIN'
            END IF
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (IMAGE, 'CPROJ', OOACAR, DIM, DUMMY, PROJ, IERR)
         IF (IERR.NE.0) GO TO 990
         MSGSUP = MSGSAV
         MSGSUP = 32000
         CALL OGET (IMAGE, 'CCENTER', TYPE, DIM, COORD, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            COORD(1) = 0.0D0
            COORD(2) = 0.0D0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       DFT?
      IF (DODFT) THEN
         CTYPEI(1)(5:8) = '-ARC'
         CTYPEI(2)(5:8) = '-ARC'
         END IF
C                                       Info from IMAGE and UV objects
      CALL OBNAME (IMAGE, IMOBJ, IERR)
C                                       Image size
      CALL OBGET (IMOBJ, 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         MSGTXT = 'U2IDES: MUST SPECIFY IMAGE SIZE'
         GO TO 995
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Cellsize
      CALL OBGET (IMOBJ, 'CELLSIZE', TYPE, DIM, CELSIZ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         MSGTXT = 'U2IDES: MUST SPECIFY CELLSIZE'
         GO TO 995
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Line or SUM channels
      MSGSUP = 32000
      CALL OBGET (IMOBJ, 'CHTYPE', TYPE, DIM, DUMMY, CHTYPE, IERR)
      MSGSUP = MSGSAV
C                                       Default = 'SUM'
      IF ((IERR.EQ.1) .OR. (CHTYPE.EQ.'    ')) THEN
         CHTYPE = 'SUM'
         DIM(1) = 4
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (IMOBJ, 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       SHIFT
      MSGSUP = 32000
      CALL OBGET (IMOBJ, 'SHIFT', TYPE, DIM, SHIFT, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = 0
      IF (IERR.EQ.1) THEN
         SHIFT(1) = 0.0
         SHIFT(2) = 0.0
         DIM(1) = 2
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (IMOBJ, 'SHIFT', OOARE, DIM, SHIFT, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       ISBEAM
      MSGSUP = 32000
      CALL OBGET (IMOBJ, 'ISBEAM', TYPE, DIM, ISBEAM, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = false
      IF (IERR.EQ.1) THEN
         ISBEAM = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (IMOBJ, 'ISBEAM', OOALOG, DIM, ISBEAM, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       UV axis descriptor
C                                       NDIM
      CALL UVDGET (UVDATA, 'NDIM', TYPE, DIM, NDIMU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NAXIS
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXISU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRVAL
      CALL UVDGET (UVDATA, 'CRVAL', TYPE, DIM, CRVALU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CDELT
      CALL UVDGET (UVDATA, 'CDELT', TYPE, DIM, CDELTU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRPIX
      CALL UVDGET (UVDATA, 'CRPIX', TYPE, DIM, CRPIXU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CROTA
      CALL UVDGET (UVDATA, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Selection in frequency from
C                                       UVDATA.
      CALL OBNAME (UVDATA, UVOBJ, IERR)
C                                       BIF
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (BIF.LE.0)) THEN
         BIF = 1
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'BIF', OOAINT, DIM, BIF, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       EIF
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (EIF.LE.0)) THEN
         EIF = BIF
         IF ((CHTYPE.EQ.'SUM') .AND. (JJLCIF.GT.0)) EIF = NAXISU(JJLCIF)
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'EIF', OOAINT, DIM, EIF, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (JJLCIF.GT.0) THEN
         IF (NAXISU(JJLCIF).NE.EIF-BIF+1) THEN
            WRITE (MSGTXT,1050) NAXISU(JJLCIF), EIF, BIF
            CALL MSGWRT (7)
            IERR = 9
            GO TO 990
            END IF
         END IF
C                                       BCHAN
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (BCHAN.LE.0)) THEN
         BCHAN = 1
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       ECHAN
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (ECHAN.LE.0)) THEN
         ECHAN = NAXISU(JJLOCF)
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (NAXISU(JJLOCF).NE.ECHAN-BCHAN+1) THEN
         WRITE (MSGTXT,1051) NAXISU(JJLOCF), ECHAN, BCHAN
         CALL MSGWRT (7)
         IERR = 9
         GO TO 990
         END IF
C                                       CHINC
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default CHINC = 1
      IF ((IERR.EQ.1) .OR. (CHINC.LE.0)) THEN
         CHINC = 1
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'CHINC', OOAINT, DIM, CHINC, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       NCHAV
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default NCHAV = 1
      IF ((IERR.EQ.1) .OR. (NCHAV.LE.0)) THEN
         NCHAV = 1
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OBPUT (UVOBJ, 'NCHAV', OOAINT, DIM, NCHAV, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       STOKES
      MSGSUP = 32000
      CALL OBGET (UVOBJ, 'STOKES', TYPE, DIM, DUMMY, STOKES, IERR)
      MSGSUP = MSGSAV
C                                       Default STOKES = 'I'
      IF ((IERR.EQ.1) .OR. (STOKES.EQ.' ')) THEN
         STOKES = 'I'
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Coords for single-dish:
C                                       0,0 standard for rel Az-El
      IF (UVTYPE.EQ.'SD') THEN
         IF ((COORD(1).EQ.0.0D0) .AND. (COORD(2).EQ.0.0)) THEN
            COORD(1) = CRVALU(JJLOCR)
            COORD(2) = CRVALU(JJLOCD)
            END IF
         IF ((COORD(1).EQ.0.0D0) .AND. (COORD(2).EQ.0.0)) THEN
            CALL PSNGET (UVDATA, 'OBSRA', TYPE, DIM, COORD(1), CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL PSNGET (UVDATA, 'OBSDEC', TYPE, DIM, COORD(2), CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      IF ((UVTYPE.EQ.'SD') .OR. (UVTYPE.EQ.'SB')) THEN
         CRVALU(JJLOCR) = COORD(1)
         CRVALU(JJLOCD) = COORD(2)
         DIM(1) = 2
         DIM(2) = 1
         CALL OPUT (IMAGE, 'CCENTER', OOADP, DIM, COORD, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Ra Axis
      NAXISI(1) = IMSIZE(1,1)
      CRVALI(1) = CRVALU(JJLOCR)
      CDELTI(1) = - ABS (CELSIZ(1)) / 3600.0
      IF (UVTYPE.EQ.'SB') CDELTI(1) = -CDELTI(1)
      CRPIXI(1) = (IMSIZE(1,1)/2) - SHIFT(1) / (3600. * CDELTI(1))
      IF (ISBEAM) CRPIXI(1) = (IMSIZE(1,1)/2)
      CROTAI(1) = 0.0
C                                       Dec Axis
      NAXISI(2) = IMSIZE(2,1)
      CRVALI(2) = CRVALU(JJLOCD)
      CDELTI(2) = ABS (CELSIZ(2)) / 3600.0
      CRPIXI(2) = IMSIZE(2,1) / 2 + 1 - SHIFT(2) / ABS (CELSIZ(2))
      IF (ISBEAM) CRPIXI(2) = ((IMSIZE(2,1)/2) + 1)
      CROTAI(2) = CROTAU(JJLOCD)
C                                       Frequency Axis
C                                       Get reference frequency
C                                       Note REFREQ corrected for BIF
C                                       & source offsets in LINE and
C                                       REFPIX corrected for NCHAV
C                                       and CHINC
C                                       in average corrected REFPIX=1
C                                       and freq average measured
      CALL UVREFQ (UVDATA, CHTYPE, REFREQ, REFPIX, IERR)
      CROTAI(3) = 0.0
C                                       Image channels
      IF (CHTYPE.EQ.'LINE') THEN
         NAXISI(3) = ((ECHAN - BCHAN + 1 - NCHAV) / CHINC) + 1
         NAXISI(3) = MAX (1, NAXISI(3))
         CDELTI(3) = CDELTU(JJLOCF) * CHINC
         CRVALI(3) = REFREQ + (1.0D0 - REFPIX) * CDELTI(3)
         CRPIXI(3) = 1.0
C                                       Sum channels
      ELSE
         MSGSUP = 32000
         CALL OBGET (IMOBJ, 'AVERFREQ', TYPE, DIM, AVFREQ, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            CRVALI(3) = REFREQ
         ELSE
            CRVALI(3) = AVFREQ
            END IF
         NAXISI(3) = 1
         CDELTI(3) = (ECHAN - BCHAN + 1) * CDELTU(JJLOCF)
         CRPIXI(3) = 1.0
         END IF
C                                       Stokes Axis
      NAXISI(4) = 1
      IF (STOKES(1:1).EQ.'I') THEN
         CRVALI(4) = 1.0D0
      ELSE IF (STOKES(1:1).EQ.'Q') THEN
         CRVALI(4) = 2.0D0
      ELSE IF (STOKES(1:1).EQ.'U') THEN
         CRVALI(4) = 3.0D0
      ELSE IF (STOKES(1:1).EQ.'V') THEN
         CRVALI(4) = 4.0D0
      ELSE IF (STOKES(1:1).EQ.'R') THEN
         CRVALI(4) = -1.0D0
      ELSE IF (STOKES(1:1).EQ.'L') THEN
         CRVALI(4) = -2.0D0
         END IF
      IF (ISBEAM) CRVALI(4) = 0.0D0
      CDELTI(4) = 1.0
      CRPIXI(4) = 1.0
      CROTAI(4) = 0.0
C                                       Update IMAGE axis descriptor
C                                       NDIM
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (IMAGE, 'NDIM', OOAINT, DIM, NDIMI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NAXIS
      DIM(1) = 7
      CALL ARDPUT (IMAGE, 'NAXIS', OOAINT, DIM, NAXISI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CTYPE
      DIM(1) = 8
      DIM(2) = 7
      CALL IMDPUT (IMAGE, 'CTYPE', OOACAR, DIM, DUMMY, CTYPEI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRVAL
      DIM(1) = 7
      DIM(2) = 1
      CALL IMDPUT (IMAGE, 'CRVAL', OOADP, DIM, CRVALI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CDELT
      CALL IMDPUT (IMAGE, 'CDELT', OOARE, DIM, CDELTI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRPIX
      CALL IMDPUT (IMAGE, 'CRPIX', OOARE, DIM, CRPIXI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CROTA
      MSGSUP = 32000
      CALL OGET (IMAGE, 'ROTATE', TYPE, DIM, IMGROT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IMGROT = 0.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CROTAI(2) = CROTAI(2) + IMGROT
      DIM(1) = 7
      CALL IMDPUT (IMAGE, 'CROTA', OOARE, DIM, CROTAI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       BUNIT
      IF (UVTYPE(1:1).EQ.'S') THEN
         BUNIT = 'K'
         IF (SDIWT.EQ.1) BUNIT = 'CONV WTS'
         IF (SDIWT.EQ.2) BUNIT = '/K/K'
      ELSE
         BUNIT = 'JY/BEAM'
         END IF
      DIM(1) = 8
      CALL IMDPUT (IMAGE, 'BUNIT', OOACAR, DIM, DUMMY, BUNIT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Velocity
      CALL VELGET (UVDATA, 'VELREF', TYPE, DIM, VELREF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELPUT (IMAGE, 'VELREF', TYPE, DIM, VELREF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELGET (UVDATA, 'ALTRVAL', TYPE, DIM, ALTVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELPUT (IMAGE, 'ALTRVAL', TYPE, DIM, ALTVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELGET (UVDATA, 'ALTRPIX', TYPE, DIM, ALTPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UVGET fixes for BCHAN
      ALTPIX = (ALTPIX  - 1.0 - (NCHAV-1.0)/2.0) / CHINC + 1.0
      CALL VELPUT (IMAGE, 'ALTRPIX', TYPE, DIM, ALTPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELGET (UVDATA, 'RESTFREQ', TYPE, DIM, RESTFQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL VELPUT (IMAGE, 'RESTFREQ', TYPE, DIM, RESTFQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Pointing Position
      CALL PSNGET (UVDATA, 'OBSRA', TYPE, DIM, OBSRA, CVAL, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNGET (UVDATA, 'OBSDEC', TYPE, DIM, OBSDEC, CVAL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       defend ourselves
      IF ((OBSRA.EQ.0.0D0) .AND. (OBSDEC.EQ.0.0D0)) THEN
         OBSRA = CRVALU(JJLOCR)
         OBSDEC = CRVALU(JJLOCD)
         END IF
      CALL PSNPUT (IMAGE, 'OBSRA', TYPE, DIM, OBSRA, CVAL, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNPUT (IMAGE, 'OBSDEC', TYPE, DIM, OBSDEC, CVAL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set for shift
      DG2RAD = 1.745329252D-2
      MAPROT = CROTAI(2) * DG2RAD
      RA = CRVALU(JJLOCR)
      DEC = CRVALU(JJLOCD)
      XSHFT = (COS (MAPROT) * SHIFT(1) - SIN (MAPROT) * SHIFT(2)) / 3600.
      YSHFT = (SIN (MAPROT) * SHIFT(1) + COS (MAPROT) * SHIFT(2)) / 3600.
      XSHFT = - XSHFT / COS (DEC * DG2RAD)
      DIM(1) = 1
      CALL PSNPUT (IMAGE, 'XSHIFT', OOARE, DIM, XSHFT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNPUT (IMAGE, 'YSHIFT', OOARE, DIM, YSHFT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       determine ref pixel for
C                                       conventional ref value
      IF ((PROJ.EQ.'-GLS') .OR. (PROJ.EQ.'-AIT') .OR. (PROJ.EQ.'-MER'))
     *   THEN
         CALL FILL (5, 1, DEPTH)
         CALL COPY (256, CATBLK, CATSAV)
         CALL OBHGET (IMAGE, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CATD(KDCRV+1) = 0.0D0
         IF ((LOCNUM.LE.0) .OR. (LOCNUM.GT.NUMLOC)) LOCNUM = 1
         CALL SETLOC (DEPTH, .TRUE.)
         CALL XYPIX (RA, DEC, XPIX, YPIX, IERR)
         IF (IERR.EQ.0) THEN
            CATR(KRCRP)   = 2.0 * CATR(KRCRP)   - XPIX
            CATR(KRCRP+1) = 2.0 * CATR(KRCRP+1) - YPIX
            CALL OBHPUT (IMAGE, CATBLK, IERR)
            IF (IERR.NE.0) GO TO 990
         ELSE
            CATD(KDCRV+1) = DEC
            IERR = 0
            END IF
         CALL COPY (256, CATSAV, CATBLK)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'U2IDES: ERROR SPECIFYING ' // IMAGE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('U2IDES: FUNNY IFs: UV, EIF BIF =',3I5)
 1051 FORMAT ('U2IDES: FUNNY CHANs: UV, ECHAN BCHAN =',3I5)
      END
      SUBROUTINE UVFRQS (UVDATA, UVFREQ, FREQS, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Returns reference (u,v,w) frequency and a table of frequencies in
C   the order that they occur in the data.
C   Honors AVERAGEF parameter (if present) unless UVFREQ on input is
C   < -999.0.
C   Inputs:
C      UDATA   C*?  Name of uvdata.
C   Output:
C      UVFREQ  D    u,v,w reference frequency
C      FREQS   D(*) Frequency as (channel,IF)
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      DOUBLE PRECISION UVFREQ, FREQS(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(3), JLOCF, JLOCIF, NAXIS(7), LOOPF, LOOPIF,
     *   NIF, FREQID, COUNT, BIF, EIF, BCHAN, ECHAN, MSGSAV, NCT, NFRQAV
      REAL      CRPIX(7), REFPIX
      CHARACTER TMPTAB*32, CDUMMY*1
      LOGICAL   NOAVG
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), SFOFF(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
      NOAVG = UVFREQ.LT.-999.0D0
C                                       Frequency info
C                                       Uv data pointers
      CALL UVDFND (UVDATA, 2, 'FREQ', JLOCF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 2, 'IF  ', JLOCIF, IERR)
C                                       May not be there
      IF (IERR.NE.0) THEN
         JLOCIF = -1
         IERR = 0
         END IF
C                                       UV axis descriptor
C                                       NAXIS
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reference frequency.
      CALL UVDGET (UVDATA, 'REFFREQ', TYPE, DIM, UVFREQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'REFFPIX', TYPE, DIM, REFPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRPIX
      CALL UVDGET (UVDATA, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       FREQID
      CALL SECGET (UVDATA, 'FRQSEL', TYPE, DIM, FREQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       frequency averaging
      IF (NOAVG) THEN
         NFRQAV = 1
      ELSE
         MSGSUP = 32000
         CALL OGET (UVDATA, 'AVERAGEF', TYPE, DIM, NFRQAV, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            NFRQAV = 1
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Make table object from UVDATA
      TMPTAB = 'Temporary FQ table for UVFRQS'
      CALL UV2TAB (UVDATA, TMPTAB, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get IF info
      CALL OCHNDA (TMPTAB, 'READ', NIF, FOFF, ISBAND, FINC, BNDCOD,
     *   FREQID, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVDATA, 'SOURFREQ', TYPE, DIM, SFOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary object
      CALL OBFREE (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       BCHAN, ECHAN
      IF (JLOCF.GE.0) THEN
         CALL OUVGET (UVDATA, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVDATA, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (ECHAN-BCHAN+1.NE.NAXIS(JLOCF)) THEN
            MSGTXT = 'UVFRQS: BCHAN, ECHAN DO NOT MATCH NAXIS(JLOCF)'
            CALL MSGWRT (7)
            ECHAN = BCHAN + NAXIS(JLOCF) - 1
            END IF
      ELSE
         BCHAN = 1
         ECHAN = 1
         END IF
C                                       BIF, EIF
      IF (JLOCIF.GE.0) THEN
         CALL OUVGET (UVDATA, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVGET (UVDATA, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         BIF = MAX (1, MIN (NIF, BIF))
         IF ((EIF.LT.BIF) .OR. (EIF.GT.NIF)) EIF = NIF
         IF (EIF-BIF+1.NE.NAXIS(JLOCIF)) THEN
            MSGTXT = 'UVFRQS: BIF, EIF DO NOT MATCH NAXIS(JLOCIF)'
            CALL MSGWRT (7)
            EIF = BIF + NAXIS(JLOCIF) - 1
            END IF
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Frequency table
C                                       Same order as data
C                                       Frequency first
      IF ((JLOCF.LT.JLOCIF) .OR. (JLOCIF.LE.0)) THEN
         DO 60 LOOPIF = BIF,EIF
            DO 50 LOOPF = BCHAN,ECHAN
               COUNT = COUNT + 1
               FREQS(COUNT) = UVFREQ + FOFF(LOOPIF) + SFOFF(LOOPIF) +
     *            (LOOPF-REFPIX) *  FINC(LOOPIF)
 50            CONTINUE
 60         CONTINUE
C                                       IF first
      ELSE
         DO 160 LOOPF = BCHAN,ECHAN
            DO 150 LOOPIF = BIF,EIF
               COUNT = COUNT + 1
               FREQS(COUNT) = UVFREQ + FOFF(LOOPIF) + SFOFF(LOOPIF) +
     *            (LOOPF-REFPIX) *  FINC(LOOPIF)
 150           CONTINUE
 160        CONTINUE
         END IF
      IF (NFRQAV.GT.1) THEN
         NCT = (EIF-BIF+1) * (ECHAN-BCHAN+1)
         COUNT = 0
         DO 180 LOOPF = 1,NCT,NFRQAV
            COUNT = COUNT + 1
            FREQS(COUNT) = FREQS(LOOPF)
            DO 170 LOOPIF = 2,NFRQAV
               FREQS(COUNT) = FREQS(COUNT) + FREQS(LOOPF+LOOPIF-1)
 170           CONTINUE
            FREQS(COUNT) = FREQS(COUNT) / NFRQAV
 180        CONTINUE
         LOOPF = NCT - COUNT
         COUNT = COUNT + 1
         CALL DFILL (LOOPF, 0.0D0, FREQS(COUNT))
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVFRQS: ERROR FINDING FREQUENCIES FOR ' // UVDATA
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVREFQ (UVDATA, CHTYPE, REFREQ, REFPIX, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Determines reference frequency and pixel for a uvdata set subject to
C   frequency selection and averaging.
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      CHTYPE  C*4  'SUM' (default) or 'LINE' to indicate if data are to
C                   be averaged in frequency.
C   Inputs from UVDATA object:
C      BIF     I    First IF selected
C      EIF     I    Highest IF selected.
C      BCHAN   I    First channel selected
C      ECHAN   I    Highest channel selected.
C      CHINC   I    Channel increment
C   Output:
C      REFREQ  D    Reference frequency (Hz) - corrected for BIF
C      REFPIX  R    Reference pixel - corrected for bchan, chinc, nchav
C      IERR    I    Error code: 0 => ok
C   The output is changed to match the averaging being done, so bchan,
C   echan, chinc, nchav, BIF, chtype, etc must all be known.
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), CHTYPE*4
      DOUBLE PRECISION REFREQ
      REAL      REFPIX
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), JLOCF, BIF, EIF, BCHAN, ECHAN, NCHAV,
     *   CHINC, FREQID, NIF, ISBAND(MAXIF), LOOPF, LOOPIF, COUNT
      REAL      CRPIX(7), FINC(MAXIF), OLDPIX
      DOUBLE PRECISION  FOFF(MAXIF), SFOFF(MAXIF), OLDFRQ, SUMFQ
      CHARACTER TMPTAB*32, STOKES*32, CDUMMY*1, BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Uv data pointers
      CALL UVDFND (UVDATA, 2, 'FREQ', JLOCF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UV axis descriptor
C                                       Previous reference frequency.
      CALL UVDGET (UVDATA, 'REFFREQ', TYPE, DIM, OLDFRQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'REFFPIX', TYPE, DIM, OLDPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRPIX
      CALL UVDGET (UVDATA, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Selection in frequency from
C                                       UVDATA.
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CHINC
      CALL OUVGET (UVDATA, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCHAV
      CALL OUVGET (UVDATA, 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       FREQID
      CALL SECGET (UVDATA, 'FRQSEL', TYPE, DIM, FREQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Make table object from UVDATA
      TMPTAB = 'Temporary FQ table for UVREFQ'
      CALL UV2TAB (UVDATA, TMPTAB, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get IF info
      CALL OCHNDA (TMPTAB, 'READ', NIF, FOFF, ISBAND, FINC, BNDCOD,
     *   FREQID, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVDATA, 'SOURFREQ', TYPE, DIM, SFOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       destroy temporary object
      CALL OBFREE (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine new reference freq.,
C                                       pixel
      IF (CHTYPE.EQ.'LINE') THEN
C                                       Not averaging in frequency.
         REFREQ = OLDFRQ + FOFF(BIF) + SFOFF(BIF)
         REFPIX = ((OLDPIX - BCHAN - (NCHAV-1.0)/2.0) / CHINC) + 1.0
C                                       Averaging in frequency, new
C                                       frequency, ref. pixel.
      ELSE
         REFPIX = 1.0
C                                       Average frequency
         COUNT = 0
         SUMFQ = 0.0D0
         DO 100 LOOPIF = BIF,EIF
            DO 50 LOOPF = BCHAN,ECHAN
               COUNT = COUNT + 1
               SUMFQ = SUMFQ + OLDFRQ + FOFF(LOOPIF) + SFOFF(LOOPIF) +
     *            (LOOPF-OLDPIX) * FINC(LOOPIF)
 50            CONTINUE
 100        CONTINUE
         IF (COUNT.GT.0) THEN
            REFREQ = SUMFQ / COUNT
         ELSE
            REFREQ = OLDFRQ
            END IF
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVREFQ: ERROR GETTING REF FREQ. ' // UVDATA
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVDTCO (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copy descriptive (AN, FQ, SU) tables from UVIN to UVOUT
C   If UVIN is UVOUT or the underlying files are the same then this
C   routine simply returns.
C      An SU table is copied only if the output uvdata has a "SOURCE"
C   random parameter.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   DISKI, CNOI, DISKO, CNOO, NUMAN, IANT, SIDOFF, JERR
      CHARACTER ANTAB*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is input output?
      IF (UVIN.EQ.UVOUT) GO TO 999
C                                       Check if underlying files are
C                                       the same.
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((DISKI.EQ.DISKO) .AND. (CNOI.EQ.CNOO)) GO TO 999
C                                       FQ - select by IF
      CALL UVFCOP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       SU - only if output has a SOURCE
C                                       random parameter.
      CALL UVDFND (UVOUT, 1, 'SOURCE  ', SIDOFF, JERR)
      IF ((JERR.EQ.0) .AND. (SIDOFF.GE.0)) THEN
         CALL UVTCOP (UVIN, UVOUT, 'SU', 1, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       How many AN tables?
      ANTAB = 'Temporary table for UVDTCO'
      CALL UV2TAB (UVIN, ANTAB, 'AN', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NUMAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temporary table object.
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop copying AN tables
      DO 100 IANT = 1,NUMAN
         CALL UVTCOP (UVIN, UVOUT, 'AN', IANT, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
      IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVDTCO: ERROR COPYING DESCRIPTIVE TABLES FROM '
     *   // UVIN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVTCOP (UVIN, UVOUT, TBTYPE, TBVER, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copy selected table from one uv data to another.
C   If UVIN is UVOUT or the underlying files are the same then this
C   routine simply returns.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object.
C      TBTYPE  C*2  Table type.
C      TBVER   I    Table version 0=> all.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*), TBTYPE*2
      INTEGER   TBVER, IERR
C
      INTEGER   TCAT(256), UCAT(256), DISKI, CNOI, DISKO, CNOO, DIM(7)
      LOGICAL   EXIST
      CHARACTER TAB1*32, TAB2*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is input output?
      IF (UVIN.EQ.UVOUT) GO TO 999
C                                       Check if underlying files are
C                                       the same.
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((DISKI.EQ.DISKO) .AND. (CNOI.EQ.CNOO)) GO TO 999
C                                       Make table objects
      TAB1 = 'Temporary Table 1 for UVTCOP'
      TAB2 = 'Temporary Table 2 for UVTCOP'
      CALL UV2TAB (UVIN, TAB1, TBTYPE, TBVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UV2TAB (UVOUT, TAB2, TBTYPE, TBVER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set outfile disk, cno
      DIM(1) = 1
      DIM(2) = 1
      CALL TABPUT (TAB2, 'DISK', OOAINT, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABPUT (TAB2, 'CNO', OOAINT, DIM, CNOO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete output table if it
C                                       exists.
      CALL OBFEXS (TAB2, EXIST, IERR)
      IERR = 0
      IF (EXIST) THEN
         CALL TABZAP (TAB2, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Recreate object
         CALL UV2TAB (UVOUT, TAB2, TBTYPE, TBVER, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy
      CALL TBLCOP (TAB1, TAB2, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Update uv CATBLK
      CALL OBHGET (UVOUT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (TAB2, TCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVTNF (TCAT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHPUT (UVOUT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temp. table objects
      CALL TABDES (TAB1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABDES (TAB2, IERR)
      IF (IERR.NE.0) GO TO 990
      IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVTCOP: ERROR COPYING ' // TBTYPE // ' TABLE FROM '
     *   // UVIN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVFCOP (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies selected portion of an FQ table from one UVDATA to another.
C   If UVIN is UVOUT then this routine simply returns.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object.
C   Inputs attached to UVIN
C      'BIF'    I    First IF selected
C      'EIF'    I    Highest IF selected
C      'FRQSEL' I    Frequency ID selected.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   TCAT(256), UCAT(256), TYPE, DIM(7), BIF, EIF, FREQID,
     *   DISKI, CNOI, DISKO, CNOO, MSGSAV, NFRQAV
      LOGICAL   EXIST
      CHARACTER TAB1*32, TAB2*32, CDUMMY*1
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION SFOFF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Is input output?
      IF (UVIN.EQ.UVOUT) GO TO 999
C                                       Check if underlying files are
C                                       the same.
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((DISKI.EQ.DISKO) .AND. (CNOI.EQ.CNOO)) GO TO 999
C                                       Get selection criteria
C                                       BIF
      CALL SECGET (UVIN, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       EIF
      CALL SECGET (UVIN, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       FREQID
      CALL SECGET (UVIN, 'FRQSEL', TYPE, DIM, FREQID, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       SFREQS
      CALL OUVGET (UVIN, 'SOURFREQ', TYPE, DIM, SFOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       frequency averaging
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'AVERAGEF', TYPE, DIM, NFRQAV, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         NFRQAV = 1
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       FQ - allow IF selection
      TAB1 = 'UVFCOP temp. table 1'
      TAB2 = 'UVFCOP temp. table 2'
      CALL UV2TAB (UVIN, TAB1, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UV2TAB (UVOUT, TAB2, 'FQ', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set outfile disk, cno
      DIM(1) = 1
      DIM(2) = 1
      CALL TABPUT (TAB2, 'DISK', OOAINT, DIM, DISKO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABPUT (TAB2, 'CNO', OOAINT, DIM, CNOO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete output table if it
C                                       exists.
      CALL OBFEXS (TAB2, EXIST, IERR)
      IERR = 0
      IF (EXIST) THEN
         CALL TABZAP (TAB2, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Recreate object
         CALL UV2TAB (UVOUT, TAB2, 'FQ', 1, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy
      CALL OCHNCO (TAB1, TAB2, BIF, EIF, NFRQAV, FREQID, SFOFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Update uv CATBLK
      CALL OBHGET (UVOUT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (TAB2, TCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVTNF (TCAT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHPUT (UVOUT, UCAT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temp. table objects
      CALL TABDES (TAB1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABDES (TAB2, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVFCOP: ERROR COPYING FQ TABLE FROM ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UV2TAB (UVDATA, TABLE, TBTYPE, TBVER, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Makes table object associated with a uvdata object.
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      TABLE   C*?  Name of table object.
C      TBTYPE  C*2  Table type.
C      TBVER   I    Table version
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), TABLE*(*), TBTYPE*2
      INTEGER   TBVER, IERR
C
      INTEGER NKEY
C                                       NKEY = number of keywords to
C                                       copy from UVDATA to TABLE
      PARAMETER (NKEY = 4)
      INTEGER   LOOP, IVAL(20), TYPE, DIM(7), DUMMY
      CHARACTER KEYWDI(NKEY)*8, KEYWDO(NKEY)*8, CVAL*20, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA KEYWDI /'NAME','CLASS','IMSEQ','DISK'/
      DATA KEYWDO /'NAME','CLASS','IMSEQ','DISK'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Make Table object
      CALL TABCRE (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy keywords
      DO 100 LOOP = 1,NKEY
         CALL OUVGET (UVDATA, KEYWDI(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABPUT (TABLE, KEYWDO(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'TBLTYPE', OOACAR, DIM, DUMMY, TBTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Table version
      DIM(1) = 1
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TBVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'UV2TAB: ERROR MAKING TABLE FROM ' // UVDATA
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVBAVG (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Baseline dependent time averaging of a uv data set.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C      UVOUT   C*?   Name of output uvdata object.
C   Inputs from UVIN Object.
C      MAXATIME R    Maximum averaging time in seconds
C      MAXFOV   R    Maximum field of view in arc min.
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'UVSTUFF.INC'
      CHARACTER SRTORD*2, CDUMMY*1, UVTYPE*2
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   CNT, LOOP, DUMMY
      REAL      RPS(50), VSS(3,MXVS), MXTAVG, MAXFOV, MXUVDF, UVDIS,
     *   U, V, TIME
      LOGICAL   NEXT, DOSU, DOFQ
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'UVBAVG: DATA ARE TYPE ''' // UVTYPE // ''' NOT UV'
         IERR = 5
         GO TO 995
         END IF
C                                       Check that sort = B*
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD.NE.'BT') THEN
         MSGTXT = 'UVBAVG: DATA NOT IN BT ORDER, USE UVSRT'
         IERR = 5
         GO TO 995
         END IF
C                                       Create output object
      CALL OCLONE (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Max averaging time (day)
      CALL OUVGET (UVIN, 'MAXATIME', TYPE, DIM, MXTAVG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MXTAVG = MXTAVG / (60.0 * 60.0 * 24.0)
C                                       Max field of view (arcmin)
      CALL OUVGET (UVIN, 'MAXFOV', TYPE, DIM, MAXFOV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Max u-v change, no more than 1%
C                                       amplitude reduction.
C                                       (sinc (0.0781))
      MXUVDF = 0.0781 / (MAXFOV * 2.908882E-4)
C                                       Get info
C                                       Uv data pointers
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which random parameters to check
      DOSU = ILOCSU.GE.1
      DOFQ = ILOCFQ.GE.1
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that it fits
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCOR, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       Clear accumulators
      CNT = 0
      DO 20 LOOP = 1,NRPARM
         RPS(LOOP) = 0
 20      CONTINUE
      U = 0.0
      V = 0.0
      TIME = 0.0
      DO 30 LOOP = 1,NCOR
         VSS(1,LOOP) = 0.0
         VSS(2,LOOP) = 0.0
         VSS(3,LOOP) = 0.0
 30      CONTINUE
C                                       Loop through input data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Previous average done?
C                                       Field of view constraint
         UVDIS = SQRT ((RP(ILOCU) - U)**2 + (RP(ILOCV) - V)**2)
         NEXT = ABS (UVDIS) .GT. MXUVDF
C                                       Exceed time?
         NEXT = NEXT .OR. ((RP(ILOCT)-TIME) .GT. MXTAVG)
C                                       New baseline
         IF (ILOCB.GT.0) THEN
            NEXT = NEXT .OR. (RP(ILOCB).NE.RPS(ILOCB))
         ELSE
            NEXT = NEXT .OR. (RP(ILOCA1).NE.RPS(ILOCA1))
            NEXT = NEXT .OR. (RP(ILOCA2).NE.RPS(ILOCA2))
            END IF
C                                       New source
         NEXT = NEXT .OR. (DOSU .AND. (RP(ILOCSU).NE.RPS(ILOCSU)))
C                                       New FQid
         NEXT = NEXT .OR. (DOFQ .AND. (RP(ILOCFQ).NE.RPS(ILOCFQ)))
C                                       Need some data
         NEXT = NEXT .AND. (CNT .GT. 0)
         IF (NEXT) THEN
C                                       Average
C                                       U
            RPS(ILOCU) = RPS(ILOCU) / CNT
C                                       V
            RPS(ILOCV) = RPS(ILOCV) / CNT
C                                       W
            RPS(ILOCW) = RPS(ILOCW) / CNT
C                                       Time
            RPS(ILOCT) = RPS(ILOCT) / CNT
C                                       Vis
            DO 150 LOOP = 1,NCOR
               IF (VSS(3,LOOP).GT.0.0) THEN
                  VSS(1,LOOP) = VSS(1,LOOP) / VSS(3,LOOP)
                  VSS(2,LOOP) = VSS(2,LOOP) / VSS(3,LOOP)
                  END IF
 150           CONTINUE
            COUNT = COUNT + 1
            CALL UVWRIT (UVOUT, RPS, VSS, IERR)
            IF (IERR.GT.0) GO TO 990
C                                       Clear accumulators
            CNT = 0
            DO 160 LOOP = 1,NRPARM
               RPS(LOOP) = 0
 160           CONTINUE
            U = 0.0
            V = 0.0
            TIME = 0.0
            DO 170 LOOP = 1,NCOR
               VSS(1,LOOP) = 0.0
               VSS(2,LOOP) = 0.0
               VSS(3,LOOP) = 0.0
 170           CONTINUE
            END IF
C                                       Accumulate
         CNT = CNT + 1
         IF (CNT.LE.1) THEN
C                                       First vis
            CALL RCOPY (NRPARM, RP, RPS)
            U = RP(ILOCU)
            V = RP(ILOCV)
            TIME = RP(ILOCT)
         ELSE
            RPS(ILOCU) = RPS(ILOCU) + RP(ILOCU)
            RPS(ILOCV) = RPS(ILOCV) + RP(ILOCV)
            RPS(ILOCW) = RPS(ILOCW) + RP(ILOCW)
            RPS(ILOCT) = RPS(ILOCT) + RP(ILOCT)
            END IF
         DO 180 LOOP = 1,NCOR
            IF (VS(3,LOOP).GT.0.0) THEN
               VSS(1,LOOP) = VSS(1,LOOP) + VS(1,LOOP) * VS(3,LOOP)
               VSS(2,LOOP) = VSS(2,LOOP) + VS(2,LOOP) * VS(3,LOOP)
               VSS(3,LOOP) = VSS(3,LOOP) + VS(3,LOOP)
               END IF
 180        CONTINUE
C                                       Loop for next vis
         GO TO 100
C                                       Done
 200  IERR = 0
      IF (CNT.GT.0) THEN
C                                       Average.
C                                       U
         RPS(ILOCU) = RPS(ILOCU) / CNT
C                                       V
         RPS(ILOCV) = RPS(ILOCV) / CNT
C                                       W
         RPS(ILOCW) = RPS(ILOCW) / CNT
C                                       Time
         RPS(ILOCT) = RPS(ILOCT) / CNT
C                                       Vis
         DO 250 LOOP = 1,NCOR
            IF (VSS(3,LOOP).GT.0) THEN
               VSS(1,LOOP) = VSS(1,LOOP) / VSS(3,LOOP)
               VSS(2,LOOP) = VSS(2,LOOP) / VSS(3,LOOP)
               END IF
 250        CONTINUE
C                                       write last accumulation
         COUNT = COUNT + 1
         CALL UVWRIT (UVOUT, RPS, VSS, IERR)
         IF (IERR.GT.0) GO TO 990
         END IF
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVBAVG: NO DATA SELECTED'
         GO TO 995
      ELSE
         WRITE (MSGTXT,1250) COUNT
         CALL MSGWRT (4)
         END IF
C                                       Close files
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVBAVG: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVBAVG: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1250 FORMAT ('UVBAVG: Wrote ',I7,' averaged visibilities')
      END
      SUBROUTINE UVTAVG (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Time average time ordered data; the result may not be strictly time
C   ordered so sorting is recommended.
C   Can only process one subarray.
C   This routine uses an I/O buffer to hold the data being averaged.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C      UVOUT   C*?   Name of output uvdata object. (may be UVIN)
C   Inputs from UVIN Object.
C      TIMEAVG  R    averaging time in seconds
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SRTORD*2, TOUT*32, ANTAB*32, CDUMMY*1, UVTYPE*2
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   CNT, LOOP, BLPNT(MXBASE), BLCNT(MXBASE), ANTOFF(MAXANT), BUFNO,
     *   MAXAN, SUBARR, I, J, PNT, BLNO, IOFF, INDEX, NUMBL, NWORD,
     *   LBASE, ANT1, ANT2, DUMMY
      REAL      RPS(50), VSS(3,MXVS), TAVG, TIME, SID, FQID, VSCNT
      LOGICAL   NEXT, DOSU, DOFQ, EXIST, SINGLE
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      SINGLE = UVTYPE(1:1).EQ.'S'
C                                       Check that sort = T*
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD(1:1).NE.'T') THEN
         MSGTXT = 'UVTAVG: DATA NOT IN T* ORDER, USE UVSRT'
         IERR = 5
         GO TO 995
         END IF
C                                       Subarray number
      CALL SECGET (UVIN, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Create AN table and use for
C                                       buffer.
      ANTAB = 'AN table for UVTAVG'
      CALL UV2TAB (UVIN, ANTAB, 'AN', SUBARR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL OBOPEN (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get buffer number
      CALL OBINFO (ANTAB, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.GT.1) GO TO 990
      IERR = 0
      IF (.NOT.EXIST) THEN
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Temporary output in case in =
C                                       out.
      TOUT = 'Temporary output for UVTAVG'
      CALL OUVCOP (UVOUT, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (TOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (TOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Averaging time (day)
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TAVG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      TAVG = TAVG / (60.0 * 60.0 * 24.0)
C                                       Reduce by 1%
      TAVG = TAVG * 0.99
C                                       Get info
C                                       Uv data pointers
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which random parameters to check
      DOSU = ILOCSU.GE.1
      DOFQ = ILOCFQ.GE.1
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that it fits
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCOR, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       How many antennas?
      CALL ANTNO (ANTAB, SUBARR, MAXAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Length of a baseline entry
      LBASE = LREC
C                                       Get baseline pointers
      PNT = 1
      IOFF = 1
      IF (.NOT.SINGLE) THEN
         DO 15 I = 1,MAXAN-1
            ANTOFF(I) = IOFF - I
            DO 10 J = I+1,MAXAN
               BLNO = ANTOFF(I) + J
               BLPNT(BLNO) = PNT
               PNT = PNT + LBASE
 10            CONTINUE
            IOFF = IOFF + (MAXAN - I)
 15         CONTINUE
         NUMBL = (MAXAN * (MAXAN - 1) ) / 2
      ELSE
         DO 20 I = 1,MAXAN
            ANTOFF(I) = I - 1
            BLPNT(I) = PNT
            PNT = PNT + LBASE
 20         CONTINUE
         NUMBL = MAXAN
         END IF
C                                       Does it all fit in the BUFFER?
      IF (PNT.GT.BUFSIZ) THEN
         MSGTXT = 'UVTAVG: WORK ARRAY TOO SMALL'
         IERR = 5
         GO TO 995
         END IF
C                                       Clear accumulators
      NWORD = NUMBL * LBASE
      CALL RFILL (NWORD, 0.0, OBUFFR(1,BUFNO))
      CALL FILL (NUMBL, 0, BLCNT)
      CNT = 0
      TIME = 0.0
      SID = 0.0
      FQID = 0.0
C                                       Loop through input data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Previous average done?
C                                       Exceed time?
         NEXT = (RP(ILOCT)-TIME) .GT. TAVG
C                                       New source
         NEXT = NEXT .OR. (DOSU .AND. (SID.NE.RPS(ILOCSU)))
C                                       New FQid
         NEXT = NEXT .OR. (DOFQ .AND. (FQID.NE.RPS(ILOCFQ)))
C                                       Need some data
         NEXT = NEXT .AND. (CNT .GT. 0)
         IF (NEXT) THEN
C                                       Average then write old
C                                       accumulations.
            DO 160 BLNO = 1,NUMBL
               IF (BLCNT(BLNO).GT.0) THEN
C                                       Pointer into OBUFFR
                  PNT = BLPNT(BLNO)
                  CALL COPY (NRPARM, OBUFFR(PNT,BUFNO), RPS)
C                                       U
                  RPS(ILOCU) = OBUFFR(PNT+ILOCU-1,BUFNO) / BLCNT(BLNO)
C                                       V
                  RPS(ILOCV) = OBUFFR(PNT+ILOCV-1,BUFNO) / BLCNT(BLNO)
C                                       W
                  RPS(ILOCW) = OBUFFR(PNT+ILOCW-1,BUFNO) / BLCNT(BLNO)
C                                       Time
                  RPS(ILOCT) = OBUFFR(PNT+ILOCT-1,BUFNO) / BLCNT(BLNO)
C                                       Vis
                  INDEX = PNT + NRPARM
                  DO 150 LOOP = 1,NCOR
                     VSCNT = OBUFFR(INDEX+2,BUFNO)
                     IF (VSCNT.GT.0) THEN
                        VSS(1,LOOP) = OBUFFR(INDEX,BUFNO) / VSCNT
                        VSS(2,LOOP) = OBUFFR(INDEX+1,BUFNO) / VSCNT
                        VSS(3,LOOP) = OBUFFR(INDEX+2,BUFNO)
                     ELSE
                        VSS(1,LOOP) = 0.0
                        VSS(2,LOOP) = 0.0
                        VSS(3,LOOP) = 0.0
                        END IF
                     INDEX = INDEX + 3
 150                 CONTINUE
                  COUNT = COUNT + 1
                  CALL UVWRIT (TOUT, RPS, VSS, IERR)
                  IF (IERR.GT.0) GO TO 990
C                                       Clear accumulators
                  CALL RFILL (LBASE, 0.0, OBUFFR(PNT,BUFNO))
                  BLCNT(BLNO) = 0
                  END IF
 160           CONTINUE
C                                       Clear no. vis this integrations
C                                       count.
            CNT = 0
            END IF
C                                       Accumulate
C                                       Antenna, baseline info
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(ILOCA1)
            ANT2 = RP(ILOCA2)
            END IF
         IF (SINGLE) THEN
            BLNO = ANT2
         ELSE
            BLNO = ANTOFF(ANT1) + ANT2
            END IF
         PNT = BLPNT(BLNO)
         BLCNT(BLNO) = BLCNT(BLNO) + 1
         CNT = CNT + 1
C                                       Save info for this integration
         IF (CNT.LE.1) THEN
            TIME = RP(ILOCT)
            SID = 1
            IF (DOSU) SID =  RP(ILOCSU)
            FQID = 1
            IF (DOFQ) FQID =  RP(ILOCFQ)
            END IF
C                                       First vis for baseline
         IF (BLCNT(BLNO).LE.1) THEN
            CALL RCOPY (NRPARM, RP, OBUFFR(PNT,BUFNO))
         ELSE
            OBUFFR(PNT-1+ILOCU,BUFNO) = OBUFFR(PNT-1+ILOCU,BUFNO) +
     *         RP(ILOCU)
            OBUFFR(PNT-1+ILOCV,BUFNO) = OBUFFR(PNT-1+ILOCV,BUFNO) +
     *         RP(ILOCV)
            OBUFFR(PNT-1+ILOCW,BUFNO) = OBUFFR(PNT-1+ILOCW,BUFNO) +
     *         RP(ILOCW)
            OBUFFR(PNT-1+ILOCT,BUFNO) = OBUFFR(PNT-1+ILOCT,BUFNO) +
     *         RP(ILOCT)
            END IF
         INDEX = PNT + NRPARM
         DO 180 LOOP = 1,NCOR
            IF (VS(3,LOOP).GT.0.0) THEN
               OBUFFR(INDEX,BUFNO) = OBUFFR(INDEX,BUFNO) +
     *            VS(1,LOOP) * VS(3,LOOP)
               OBUFFR(INDEX+1,BUFNO) = OBUFFR(INDEX+1,BUFNO) +
     *            VS(2,LOOP) * VS(3,LOOP)
               OBUFFR(INDEX+2,BUFNO) = OBUFFR(INDEX+2,BUFNO) +
     *            VS(3,LOOP)
               END IF
            INDEX = INDEX + 3
 180        CONTINUE
C                                       Loop for next vis
         GO TO 100
C                                       Done
 200  IERR = 0
C                                       Average and write last set.
      DO 260 BLNO = 1,NUMBL
         IF (BLCNT(BLNO).GT.0) THEN
C                                       Pointer into OBUFFR
            PNT = BLPNT(BLNO)
            CALL COPY (NRPARM, OBUFFR(PNT,BUFNO), RPS)
C                                       U
            RPS(ILOCU) = OBUFFR(PNT+ILOCU-1,BUFNO) / BLCNT(BLNO)
C                                       V
            RPS(ILOCV) = OBUFFR(PNT+ILOCV-1,BUFNO) / BLCNT(BLNO)
C                                       W
            RPS(ILOCW) = OBUFFR(PNT+ILOCW-1,BUFNO) / BLCNT(BLNO)
C                                       Time
            RPS(ILOCT) = OBUFFR(PNT+ILOCT-1,BUFNO) / BLCNT(BLNO)
C                                       Vis
            INDEX = PNT + NRPARM
            DO 250 LOOP = 1,NCOR
               VSCNT = OBUFFR(INDEX+2,BUFNO)
               IF (VSCNT.GT.0) THEN
                  VSS(1,LOOP) = OBUFFR(INDEX,BUFNO) / VSCNT
                  VSS(2,LOOP) = OBUFFR(INDEX+1,BUFNO) / VSCNT
                  VSS(3,LOOP) = OBUFFR(INDEX+2,BUFNO)
               ELSE
                  VSS(1,LOOP) = 0.0
                  VSS(2,LOOP) = 0.0
                  VSS(3,LOOP) = 0.0
                  END IF
               INDEX = INDEX + 3
 250        CONTINUE
            COUNT = COUNT + 1
            CALL UVWRIT (TOUT, RPS, VSS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
 260     CONTINUE
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'UVTAVG: NO DATA SELECTED'
         GO TO 995
      ELSE
         WRITE (MSGTXT,1250) COUNT
         CALL MSGWRT (4)
         END IF
C                                       Set sort order (unsorted)
      SRTORD = '  '
      DIM(1) = LEN (SRTORD)
      DIM(2) = 1
      CALL UVDPUT (TOUT, 'SORTORD', OOACAR, DIM, DUMMY, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy tables
      CALL UVDTCO (UVIN, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary objects
      CALL OUVDES (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'UVTAVG: ERROR AVERAGING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVTAVG: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1250 FORMAT ('UVTAVG: Wrote ',I7,' averaged visibilities')
      END
      SUBROUTINE UVCALT (UVIN, UVOUT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Copies all tables from one uvdata object to another except those
C   listed in optional keyword DROPTABS (<= 20 char*2 values)
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, BUFIN, BUFOUT,
     *   NDRPTB, MDRPTB, TYPE, DIM(7), IDUMMY, MSGSAV, I
      PARAMETER (MDRPTB=20)
      CHARACTER DRPTBS(MDRPTB)*2
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open object for buffer
      CALL OBOPEN (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBOPEN (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get buffer number
      CALL OBINFO (UVIN, BUFIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (UVOUT, BUFOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get other info
      CALL OBDSKC (UVIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (UVOUT, DISKO, CNOO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (UVOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Assign LUNs
      CALL OBLUN (LUNI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get table list not to copy
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'DROPTABS', TYPE, DIM, IDUMMY, DRPTBS, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         NDRPTB = 0
         DRPTBS(1) = '  '
      ELSE
         NDRPTB = 0
         I = MAX (1, MIN (MDRPTB, DIM(2)))
 10      CONTINUE
         IF ((DRPTBS(NDRPTB+1).NE.'  ') .AND. (NDRPTB.LT.I)) THEN
            NDRPTB = NDRPTB + 1
            GO TO 10
            END IF
         END IF
C                                       Copy table
      CALL ALLTAB (NDRPTB, DRPTBS, LUNI, LUNO, DISKI, DISKO, CNOI,
     *   CNOO, CATBLK, OBUFFR(1,BUFIN), OBUFFR(1,BUFOUT), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save catalog header
      CALL OBHPUT (UVOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close object
      CALL OBCLOS (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Free LUN
      CALL OBLUFR (LUNI)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUFR (LUNO)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'UVCALT: ERROR COPYING TABLES FOR ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVAMPS (UVIN, UVRAN, NAMPS, AMPS, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Optionally determine the range of baseline lengths and  an array of
C   the averaged amplitudes for a range of baseline length bins.  All
C   selected  channels, polarizations and IF are vector averaged in each
C   visibility but difference visibilities are scalar averaged.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C   Input/output:
C      UVRAN   R(2) Range of baseline lengths in kilowavelengths.
C                   If they are nonzero on input only this range of
C                   baseline lengths are used; else the actual range is
C                   selected.
C      NAMPS   I    Number of amplitude bins.  .le. 1 > don't compute
C                   AMPS.
C   Output:
C      AMPS    R(5,*)  (1,*) = average amplitude
C                      (2,*) = RMS of amplitudes
C                      (3,*) = central uv distance (klamda),
C                      (4,*) = sum of weights in bin
C                      (5,*) Number of amplitudes averaged.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*)
      REAL      UVRAN(2), AMPS(5,*)
      INTEGER   NAMPS, IERR
C
      INTEGER   TYPE, DIM(7), INDXU, INDXV, COUNT, ICELL, NDIM, I,
     *   NAXIS(7), LIMS(2,7), D(7), DUMMY
      LOGICAL   DORANG, DOAMP
      REAL      UVDIS, CELL, VIS(3), AMP, ARG, SUM, SUM2, CNT, WT
      CHARACTER CDUMMY*1, UVTYPE*2
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       What needs to be done?
      DORANG = ((UVRAN(1).LE.0.0) .AND. (UVRAN(2).LE.0.0))
      DOAMP = NAMPS .GT. 1
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'UVAMPS DOES NOT WORK ON ''' // UVTYPE // ''' DATA'
         IERR = 5
         GO TO 990
         END IF
C                                       Get u, v indices
      CALL UVDFND (UVIN, 1, 'UU-L', INDXU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
         GO TO 990
         END IF
      CALL UVDFND (UVIN, 1, 'VV-L', INDXV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
         GO TO 990
         END IF
C                                       Find uv range if necessary
      IF (DORANG) THEN
         COUNT = 0
         UVRAN(1) = 1.0E20
         UVRAN(2) = -1.0E20
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 995
         COUNT = COUNT + 1
C                                       Get uv distance
         UVDIS = (RP(INDXU)*RP(INDXU) + RP(INDXV)*RP(INDXV))
         IF (UVDIS.GT.0.0) UVDIS = SQRT (UVDIS) * 1.0E-3
         UVRAN(1) = MIN (UVRAN(1), UVDIS)
         UVRAN(2) = MAX (UVRAN(2), UVDIS)
         GO TO 100
 200     IERR = 0
C                                       Better be some data
         IF (COUNT.LE.0) THEN
            IERR = 7
            MSGTXT = 'UVAMPS: NO DATA SELECTED'
            GO TO 990
            END IF
         END IF
C                                       May ned to reinit I/O
      IF (DORANG .AND. DOAMP) THEN
         CALL OUVINI (UVIN, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Make pass to get AMPS
      IF (DOAMP) THEN
         COUNT = 0
C                                       Get vis info for averaging.
         CALL UVDGET (UVIN, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL FILL (5, 1, D)
         CALL FILL (10, 0, LIMS)
         DO 230 I = 1,NDIM
            D(I) = NAXIS(I)
            LIMS(1,I) = 1
            LIMS(2,I) = NAXIS(I)
 230        CONTINUE
C                                       Compute central uv distances
         CELL = (UVRAN(2) - UVRAN(1)) / NAMPS
         IF (CELL.LE.1.0E-10) CELL = 1.0
         DO 250 I = 1,NAMPS
            AMPS(1,I) = 0.0
            AMPS(2,I) = 0.0
            AMPS(3,I) = UVRAN(1) + (I - 0.5) * CELL
            AMPS(4,I) = 0.0
            AMPS(5,I) = 0.0
 250        CONTINUE
C                                       Loop thru data
 300     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.GT.0) THEN
            GO TO 995
         ELSE IF (IERR.EQ.0) THEN
            COUNT = COUNT + 1
C                                       Get uv distance
            UVDIS = (RP(INDXU)*RP(INDXU) + RP(INDXV)*RP(INDXV))
            IF (UVDIS.GT.0.0) UVDIS = SQRT (UVDIS) * 1.0E-3
C                                       Compute index
            ICELL = (UVDIS - UVRAN(1)) / CELL + 0.999
C                                       Wanted - average if valid
C                                       Average vis
            IF ((ICELL.GT.0) .AND. (ICELL.LE.NAMPS)) THEN
               CALL AVGVIS (D(2), D(3), D(4), D(5), LIMS, VS, VIS)
               IF (VIS(3).GT.0.0) THEN
                  AMP =  SQRT (VIS(1)*VIS(1) + VIS(2)*VIS(2))
                  AMPS(1,ICELL) = AMPS(1,ICELL) + AMP * VIS(3)
                  AMPS(2,ICELL) = AMPS(2,ICELL) + AMP*AMP * VIS(3)
                  AMPS(4,ICELL) = AMPS(4,ICELL) + VIS(3)
                  AMPS(5,ICELL) = AMPS(5,ICELL) + 1.0
                  END IF
               END IF
            GO TO 300
            END IF
C                                       EOF reached
         IERR = 0
C                                       Better be some data
         IF (COUNT.LE.0) THEN
            IERR = 7
            MSGTXT = 'UVAMPS: NO DATA SELECTED'
            GO TO 990
            END IF
         END IF
C                                       Average in bins
         DO 550 ICELL = 1,NAMPS
            SUM = AMPS(1,ICELL)
            SUM2 = AMPS(2,ICELL)
            WT = AMPS(4,ICELL)
            CNT = AMPS(5,ICELL)
            IF (WT.GT.0.0) THEN
               AMPS(1,ICELL) = SUM / WT
C                                       RMS of population
               IF (CNT.GT.1.9) THEN
                  ARG = (SUM2 - ((SUM**2) / WT)) / (CNT-1.0)
                  AMPS(2,ICELL) = SQRT (ABS (ARG))
               ELSE
C                                       If too little data set RMS to
C                                       average
                  AMPS(2,ICELL) = AMPS(1,ICELL)
                  END IF
               END IF
 550        CONTINUE
C                                       Close object
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'UVAMPS: ERROR GETTING STATS FOR ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVFRSC (UVNAME, SNNAME, IERR)
C-----------------------------------------------------------------------
C   This routine computes Faraday rotation self calibration solutions
C   for a uv data set and returns the results in an SN table object.
C   The uv data should already be divided by the appropriate model(s).
C   Only a single subarray at a time can be processed.
C      If the contents of the SN table object exist a new table is
C   created.
C   Inputs:
C      UVNAME C*? Name of input uv data object.
C      SNNAME C*? Name of output Table object.
C   Inputs from UVNAME (defaulted if not present):
C      SUBA          I    Selected subarray (default 1)
C      SOLINT        R    Solution interval (min). (default 1 sec)
C      SNRMIN        R    Minimum acceptable SNR (5)
C      RATMIN        R    Minimum data ration to include in solution.
C      RATMAX        R    Maximum data ration to include in solution.
C      MINNO         I    Min. no. antennas. (default 4)
C      ANTWT         R(*) Antenna weights. (default 1.0)
C      UVR_FULL      R(2) Range of baseline lengths with full weight
C                         (kilolamda). 0s => all baselines
C      WTUV          R    Weight outside of UVRANG. (default 1.0)
C      PRTLV         I    Print level (default no print)
C   Output:
C      IERR   I   Return code, 0=> OK else failed
C-----------------------------------------------------------------------
      CHARACTER UVNAME*(*), SNNAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUBA, MINNO, PRTLV
      LOGICAL   AVGIF
      REAL      SOLINT, SNRMIN, RATMIN, RATMAX, ANTWT(MAXANT),
     *   UVRANG(2), WTUV
C
      INTEGER   TYPE, DIM(7), MSGSAV, NAXIS(7), INDXIF, INDXP, NIF,
     *   NPOLN, MAXAN, SID, FQID, CNTMGM, KDAY, KHR, KMN, KSEC, NUMNOD,
     *   SNROW, NP, IANT, NODENO, COLS(2), NNIF, REFAN(2,MAXIF)
      LOGICAL   DONE, GOTANT(MAXANT), EXIST, ISAPPL, AVGPOL
      REAL      CIMAG(2,MAXIF,MAXANT), CREAL(2,MAXIF,MAXANT),
     *   CWT(2,MAXIF,MAXANT), TIMEI, GMMOD, MBDELY(2), CDELY(2,MAXIF),
     *   CRATE(2,MAXIF), IFR, DUMMY, DISP(2), DDISP(2)
      DOUBLE PRECISION TIMEC, SUMMGM, TIMEX, RANOD(1), DECNOD(1)
      CHARACTER ANTAB*32, OPCODE*4, STOKES*4, ATCOLS(2)*24, CDUMMY*1,
     *   UVTYPE*2
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NUMNOD, RANOD, DECNOD /1, 2*0.0D0/
      DATA ISAPPL /.FALSE./
      DATA ATCOLS /'TIME','ANTENNA NO.'/
C-----------------------------------------------------------------------
      IERR = 0
      SUMMGM = 0.0
      CNTMGM = 0
      MSGSAV = MSGSUP
C                                       Get parameters from uvdata
C                                       Supress messages
      MSGSUP = 32000
C                                       SOLINT
      CALL OUVGET (UVNAME, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SOLINT = 1.0 / 60.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Convert from min to days
      SOLINT = SOLINT / 1440.0
C                                       SNRMIN
      CALL OUVGET (UVNAME, 'SNRMIN', TYPE, DIM, SNRMIN, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNRMIN = 5.0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNRMIN.LE.1.0E-10) SNRMIN = 5.0
C                                       RATMIN
      CALL OUVGET (UVNAME, 'RATMIN', TYPE, DIM, RATMIN, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         RATMIN = 0.75
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (RATMIN.LE.1.0E-10) RATMIN = 0.75
C                                       RATMAX
      CALL OUVGET (UVNAME, 'RATMAX', TYPE, DIM, RATMAX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         RATMAX = 1.5
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (RATMAX.LE.1.0E-10) RATMAX = 1.5
C                                       MINNO
      CALL OUVGET (UVNAME, 'MINNO', TYPE, DIM, MINNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MINNO = 4
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       AVGIF
      CALL OUVGET (UVNAME, 'AVGIF', TYPE, DIM, AVGIF, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         AVGIF = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       ANTWT
      CALL OUVGET (UVNAME, 'ANTWT', TYPE, DIM, ANTWT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL RFILL (MAXANT, 1.0, ANTWT)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       UVR_FULL
      CALL OUVGET (UVNAME, 'UVR_FULL', TYPE, DIM, UVRANG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         UVRANG(1) = 0.0
         UVRANG(2) = 1.0E15
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       WTUV
      CALL OUVGET (UVNAME, 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         WTUV = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       PRTLV
      CALL OUVGET (UVNAME, 'PRTLV', TYPE, DIM, PRTLV, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         PRTLV = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Resume messages
      MSGSUP = MSGSAV
C                                       Set Stokes' to what's in the
C                                       object.
      STOKES = '    '
      DIM(1) = 4
      DIM(2) = 1
      CALL OUVPUT (UVNAME, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open to update info
      CALL OUVOPN (UVNAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (UVNAME, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'UVFRSC DOES NOT WORK ON ''' // UVTYPE // ''' DATA'
         IERR = 5
         GO TO 990
         END IF
C                                       Subarray (can only do one at a
C                                       time).
      CALL OUVGET (UVNAME, 'CALEDIT.SUBARR', TYPE, DIM, SUBA, CDUMMY,
     *   IERR)
      IF ((IERR.EQ.1) .OR. (SUBA.LE.0)) THEN
         SUBA = 1
         CALL OUVPUT (UVNAME, 'CALEDIT.SUBARR', OOAINT, DIM, SUBA,
     *      CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Get Vis info
      CALL UVDGET (UVNAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       IF axis
      CALL UVDFND (UVNAME, 2, 'IF  ', INDXIF, IERR)
C                                       May not be there
      IF (IERR.NE.0) THEN
         INDXIF = -1
         IERR = 0
         END IF
      IF (INDXIF.GT.0) THEN
         NIF = NAXIS(INDXIF)
         NNIF = NIF
      ELSE
         NIF = 1
         NNIF = NIF
         END IF
      IF (AVGIF) NIF = 1
C                                       STOKES axis
      CALL UVDFND (UVNAME, 2, 'STOKES', INDXP, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING STOKES AXIS'
         GO TO 990
         END IF
      NAXIS(INDXP) = MIN (NAXIS(INDXP), 4)
      NPOLN = NAXIS(INDXP)
      AVGPOL = .FALSE.
C                                       Close uvdata
      CALL OUVCLO (UVNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Insure new SN table for first
C                                       subarray.
      CALL OBFEXS (SNNAME, EXIST, IERR)
      IF (EXIST .AND. (IERR.EQ.0) .AND. (SUBA.LE.1)) THEN
C                                       Force new table
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (SNNAME, 'VER', OOAINT, DIM, 0, CDUMMY, IERR)
         IF (IERR.GT.0) GO TO 995
         END IF
      IERR = 0
C                                       Maximum antenna number.
      ANTAB = 'Tempory table for UVFRSC'
      CALL UV2TAB (UVNAME, ANTAB, 'AN', SUBA, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ANTNO (ANTAB, 1, MAXAN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check array size
      IF (BIGVIS.LT.(4*NPOLN*NIF*MAXAN*MAXAN)) THEN
         IERR = 5
         MSGTXT = 'UVFRSC: INTERNAL ARRAYS TOO SMALL'
         GO TO 990
         END IF
      OPCODE = 'OPEN'
C                                       Loop until done
 100     CALL NXTAVG (OPCODE, UVNAME, SOLINT, AVGIF, AVGPOL, ANTWT,
     *      UVRANG, WTUV, MAXAN, NIF, NPOLN, TIMEC, TIMEI, SID, FQID,
     *      BVIS, IERR)
C                                       Done?
         DONE = IERR.LT.0
         IF (DONE) IERR = 0
         IF (IERR.NE.0) GO TO 995
C                                       Open SN object
         IF (OPCODE.EQ.'OPEN') THEN
            NP = MIN (NAXIS(INDXP), 2)
            GMMOD = 1.0
            CALL OSNINI (SNNAME, 'WRIT', SNROW, MAXAN, NP, NNIF, NUMNOD,
     *         GMMOD, RANOD, DECNOD, ISAPPL, IERR)
            IF (IERR.GT.0) GO TO 995
            END IF
         OPCODE = 'READ'
C                                       Write time if requested
         IF (PRTLV.GE.1) THEN
            KDAY = TIMEC
            TIMEX = (TIMEC - KDAY) * 24.
            KHR = TIMEX
            TIMEX = (TIMEX - KHR) * 60.
            KMN = TIMEX
            TIMEX = (TIMEX - KMN) * 60.
            KSEC = TIMEX + 0.5D0
            WRITE (MSGTXT,2001) KDAY, KHR, KMN, KSEC
            CALL MSGWRT (2)
            END IF
C                                       Do solutions
         CALL FRSOLN (BVIS, MAXAN, NIF, NPOLN, NNIF, AVGIF, SNRMIN,
     *      RATMIN, RATMAX, PRTLV, CREAL, CIMAG, CWT, REFAN, GOTANT)
C                                       Write solutions to SN object
         CALL SLFSOU (SNNAME, SNROW, MAXAN, NNIF, NP, TIMEC,
     *      TIMEI, SUBA, SID, FQID, CREAL, CIMAG, CWT, REFAN, GOTANT,
     *      SUMMGM, CNTMGM, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Loop for more
         IF (.NOT.DONE) GO TO 100
C                                       Close uvdata
         CALL NXTAVG ('CLOS', UVNAME, SOLINT, AVGIF, AVGPOL, ANTWT,
     *      UVRANG, WTUV, MAXAN, NIF, NPOLN, TIMEC, TIMEI, SID, FQID,
     *      BVIS, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Close table
         CALL OTABSN (SNNAME, 'CLOS', SNROW, NP, TIMEC, TIMEI, SID,
     *      IANT, SUBA, FQID, IFR, NODENO, MBDELY, DISP, DDISP, CREAL,
     *      CIMAG, CDELY, CRATE, CWT, REFAN, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Open table object
         CALL TABOPN (SNNAME, 'WRIT', IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Set sort, time, ant if suba 1
C                                       else unsorted
         IF (SUBA.EQ.1) THEN
            CALL TABCOL (SNNAME, 2, ATCOLS, COLS, IERR)
            IF (IERR.GT.0) GO TO 995
         ELSE
            COLS(1) = 0
            COLS(2) = 0
            END IF
         DIM(1) = 2
         DIM(2) = 1
         CALL TABPUT (SNNAME, 'SORT', OOAINT, DIM, COLS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Close table object
         CALL TABCLO (SNNAME, IERR)
         IF (IERR.GT.0) GO TO 995
      GO TO 999
C                                       Error
 990  MSGSUP = MSGSAV
      CALL MSGWRT (7)
 995  MSGSUP = MSGSAV
      MSGTXT = 'UVFRSC: ERROR CALIBRATING ' // UVNAME
      CALL MSGWRT (7)
C
 999  MSGSUP = MSGSAV
      RETURN
C-----------------------------------------------------------------------
 2001 FORMAT (' Time=',I4,'/',3I3)
      END
      SUBROUTINE SLFCAL (UVNAME, SNNAME, IERR)
C-----------------------------------------------------------------------
C   This routine computes self calibration solutions for a uv data set
C   and returns the results in an SN table object.
C   The uv data should already be divided by the appropriate model(s).
C   Only a single subarray at a time can be processed.
C      If the contents of the SN table object exist a new table is
C   created.
C      NOTE: in general SLFREF should be used after SLFCAL if coherence
C   of the two polarizations is needed.
C   Inputs:
C      UVNAME C*? Name of input uv data object.
C      SNNAME C*? Name of output Table object.
C   Inputs from UVNAME (defaulted if not present):
C      SUBA          I    Selected subarray (default 1)
C      SOLINT        R    Solution interval (min). (default 1 sec)
C      REFANT        I    Ref ant to use. (default 1)
C      AVGPOL        L    True if RR and LL to be averaged (false)
C      AVGIF         L    True if all IFs to be averaged (false)
C      SNRMIN        R    Minimum acceptable SNR (5)
C      DOMGM         L    True then find the mean gain modulus and save
C                         it (true)
C      SOLTYPE       C*4  Solution type '  ', 'L1', 'GCON' (' ')
C      SOLMODE       C*4  Solution mode: 'A&P', 'P', 'P!A', 'GCON' ('P')
C                         - with R appended use robust methods
C      MINNO         I    Min. no. antennas. (default 4)
C      GAINERR       R(*) Gain error for constraints. (no default if
C                         needed)
C      SOLCON        R    Factor for penalty term (no default if
C                         needed).
C      ANTWT         R(*) Antenna weights. (default 1.0)
C      UVR_FULL      R(2) Range of baseline lengths with full weight
C                         (kilolamda). 0s => all baselines
C      WTUV          R    Weight outside of UVRANG. (default 1.0)
C      PRTLV         I    Print level (default no print)
C
C   Output:
C      IERR   I   Return code, 0=> OK else failed
C-----------------------------------------------------------------------
      CHARACTER UVNAME*(*), SNNAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUBA, REFANT, MODE, MINNO, PRTLV
      LOGICAL   AVGPOL, AVGIF, DOMGM, DOL1, DOGCON
      REAL      SOLINT, SNRMIN, GAERR(MAXANT), SOLCON, ANTWT(MAXANT),
     *   UVRANG(2), WTUV
C
      INTEGER   TYPE, DIM(7), MSGSAV, NAXIS(7), INDXIF, INDXP, NIF,
     *   NPOLN, MAXAN, REFAN(2,MAXIF), SID, FQID, CNTMGM, KDAY, KHR,
     *   KMN, KSEC, NUMNOD, SNROW, NP, IANT, NODENO, COLS(2), NNIF,
     *   DUMMY, LMODE, LWT
      LOGICAL   DONE, GOTANT(MAXANT), EXIST, ISAPPL
      REAL      CIMAG(2,MAXIF,MAXANT), CREAL(2,MAXIF,MAXANT),
     *   CWT(2,MAXIF,MAXANT), TIMEI, GMMOD, MBDELY(2), CDELY(2,MAXIF),
     *   CRATE(2,MAXIF), IFR, FFLIM, FFLAST, DISP(2), DDISP(2)
      DOUBLE PRECISION TIMEC, SUMMGM, TIMEX, RANOD(1), DECNOD(1)
      CHARACTER ANTAB*32, OPCODE*4, SOLTYP*4, SOLMOD*4, STOKES*4,
     *   ATCOLS(2)*24 , CDUMMY*1, UVTYPE*2
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'UVSTUFF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NUMNOD, RANOD, DECNOD /1, 2*0.0D0/
      DATA ISAPPL /.FALSE./
      DATA ATCOLS /'TIME','ANTENNA NO.'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Mean gain modulus statistics
      SUMMGM = 0.0D0
      CNTMGM = 0
      MSGSAV = MSGSUP
C                                       Get parameters from uvdata
C                                       Supress messages
      MSGSUP = 32000
C                                       Re-weighting factor
      CALL OUVGET (UVNAME, 'WEIGHTIT', TYPE, DIM, LWT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         LWT = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       lower limit on each iteration
      CALL OUVGET (UVNAME, 'FFLIM', TYPE, DIM, FFLAST, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FFLIM = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       last iteration clip factor
      CALL OUVGET (UVNAME, 'FFLAST', TYPE, DIM, FFLAST, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FFLAST = 2.5
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       SOLINT
      CALL OUVGET (UVNAME, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SOLINT = 1.0 / 60.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Convert from min to days
      SOLINT = SOLINT / 1440.0
C                                       REFANT
      CALL OUVGET (UVNAME, 'REFANT', TYPE, DIM, REFANT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         REFANT = 1
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       AVGPOL
      CALL OUVGET (UVNAME, 'AVGPOL', TYPE, DIM, AVGPOL, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         AVGPOL = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       AVGIF
      CALL OUVGET (UVNAME, 'AVGIF', TYPE, DIM, AVGIF, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         AVGIF = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       SNRMIN
      CALL OUVGET (UVNAME, 'SNRMIN', TYPE, DIM, SNRMIN, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNRMIN = 5.0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNRMIN.LE.1.0E-10) SNRMIN = 5.0
C                                       DOMGM
      CALL OUVGET (UVNAME, 'DOMGM', TYPE, DIM, DOMGM, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DOMGM = .TRUE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       SOLTYPE
      CALL OUVGET (UVNAME, 'SOLTYPE', TYPE, DIM, DUMMY, SOLTYP, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SOLTYP = '   '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       SOLMODE
      CALL OUVGET (UVNAME, 'SOLMODE', TYPE, DIM, DUMMY, SOLMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SOLMOD = 'P'
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       MINNO
      CALL OUVGET (UVNAME, 'MINNO', TYPE, DIM, MINNO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MINNO = 4
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       GAINERR
      CALL OUVGET (UVNAME, 'GAINERR', TYPE, DIM, GAERR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL RFILL (MAXANT, 0.0, GAERR)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       SOLCON
      CALL OUVGET (UVNAME, 'SOLCON', TYPE, DIM, SOLCON, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SOLCON = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       ANTWT
      CALL OUVGET (UVNAME, 'ANTWT', TYPE, DIM, ANTWT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL RFILL (MAXANT, 1.0, ANTWT)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       UVR_FULL
      CALL OUVGET (UVNAME, 'UVR_FULL', TYPE, DIM, UVRANG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         UVRANG(1) = 0.0
         UVRANG(2) = 1.0E15
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       WTUV
      CALL OUVGET (UVNAME, 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         WTUV = 1.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       PRTLV
      CALL OUVGET (UVNAME, 'PRTLV', TYPE, DIM, PRTLV, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         PRTLV = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Resume messages
      MSGSUP = MSGSAV
C                                       Set Stokes' to what's in the
C                                       object.
      STOKES = '    '
      DIM(1) = 4
      DIM(2) = 1
      CALL OUVPUT (UVNAME, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open to update info
      CALL OUVOPN (UVNAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (UVNAME, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'SLFCAL DOES NOT WORK ON ''' // UVTYPE // ''' DATA'
         IERR = 5
         GO TO 990
         END IF
C                                       Subarray (can only do one at a
C                                       time).
      CALL OUVGET (UVNAME, 'CALEDIT.SUBARR', TYPE, DIM, SUBA, CDUMMY,
     *   IERR)
      IF ((IERR.EQ.1) .OR. (SUBA.LE.0)) THEN
         SUBA = 1
         CALL OUVPUT (UVNAME, 'CALEDIT.SUBARR', OOAINT, DIM, SUBA,
     *      CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Digest SOLMODE and SOLTYPE
      DOL1 = SOLTYP(:2).EQ.'L1'
      DOGCON = SOLTYP(:3).EQ.'GCO'
      LMODE = 1
      IF (SOLMOD.EQ.'A&P ') LMODE = 0
      IF (SOLMOD.EQ.'P   ') LMODE = 1
      IF (SOLMOD.EQ.'P!A ') LMODE = 2
      IF (SOLMOD.EQ.'GCON') LMODE = 3
      MODE = LMODE + 10*LWT
      IF ((SOLTYP.EQ.'R') .OR. (SOLTYP.EQ.'L1R') .OR.
     *   (SOLTYP.EQ.'GCOR')) MODE = MODE + 4
C                                       Get Vis info
      CALL UVDGET (UVNAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       IF axis
      CALL UVDFND (UVNAME, 2, 'IF  ', INDXIF, IERR)
C                                       May not be there
      IF (IERR.NE.0) THEN
         INDXIF = -1
         IERR = 0
         END IF
      IF (INDXIF.GT.0) THEN
         NIF = NAXIS(INDXIF)
         NNIF = NIF
      ELSE
         NIF = 1
         NNIF = NIF
         END IF
      IF (AVGIF) NIF = 1
C                                       STOKES axis
      CALL UVDFND (UVNAME, 2, 'STOKES', INDXP, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING STOKES AXIS'
         GO TO 990
         END IF
      NAXIS(INDXP) = MIN (NAXIS(INDXP), 2)
      NPOLN = NAXIS(INDXP)
      AVGPOL = AVGPOL .OR. (NPOLN.LE.1)
      IF (AVGPOL) NPOLN = 1
C                                       Close uvdata
      CALL OUVCLO (UVNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Insure new SN table for first
C                                       subarray.
      CALL OBFEXS (SNNAME, EXIST, IERR)
      IF (EXIST .AND. (IERR.EQ.0) .AND. (SUBA.LE.1)) THEN
C                                       Force new table
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL TABPUT (SNNAME, 'VER', OOAINT, DIM, 0, CDUMMY, IERR)
         IF (IERR.GT.0) GO TO 995
         END IF
      IERR = 0
C                                       Maximum antenna number.
      ANTAB = 'Tempory table for SLFCAL'
      CALL UV2TAB (UVNAME, ANTAB, 'AN', SUBA, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ANTNO (ANTAB, 1, MAXAN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check array size
      IF (BIGVIS.LT.(4*NPOLN*NIF*MAXAN*MAXAN)) THEN
         IERR = 5
         MSGTXT = 'SLFCAL: INTERNAL ARRAYS TOO SMALL'
         GO TO 990
         END IF
C                                       Set Stokes'
      STOKES = 'HALF'
      DIM(1) = 4
      DIM(2) = 1
      CALL OUVPUT (UVNAME, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      OPCODE = 'OPEN'
C                                       Loop until done
 100     CALL NXTAVG (OPCODE, UVNAME, SOLINT, AVGIF, AVGPOL, ANTWT,
     *      UVRANG, WTUV, MAXAN, NIF, NPOLN, TIMEC, TIMEI, SID, FQID,
     *      BVIS, IERR)
C                                       Done?
         DONE = IERR.LT.0
         IF (DONE) IERR = 0
         IF (IERR.NE.0) GO TO 995
C                                       Open SN object
         IF (OPCODE.EQ.'OPEN') THEN
            NP = MIN (NAXIS(INDXP), 2)
            GMMOD = 1.0
            CALL OSNINI (SNNAME, 'WRIT', SNROW, MAXAN, NP, NNIF, NUMNOD,
     *         GMMOD, RANOD, DECNOD, ISAPPL, IERR)
            IF (IERR.GT.0) GO TO 995
            END IF
         OPCODE = 'READ'
C                                       Write time if requested
         IF (PRTLV.GE.1) THEN
            KDAY = TIMEC
            TIMEX = (TIMEC - KDAY) * 24.
            KHR = TIMEX
            TIMEX = (TIMEX - KHR) * 60.
            KMN = TIMEX
            TIMEX = (TIMEX - KMN) * 60.
            KSEC = TIMEX + 0.5D0
            WRITE (MSGTXT,2001) KDAY, KHR, KMN, KSEC
            CALL MSGWRT (2)
            END IF
C                                       Do solutions
         CALL SLFPA (BVIS, MAXAN, NIF, NAXIS(INDXP), NNIF, REFANT,
     *      AVGIF, AVGPOL, DOL1, DOGCON, GAERR, MODE, MINNO, SOLCON,
     *      SNRMIN, PRTLV, FFLIM, FFLAST, CREAL, CIMAG, CWT, REFAN,
     *      GOTANT)
C                                       Write solutions to SN object
         CALL SLFSOU (SNNAME, SNROW, MAXAN, NNIF, NP, TIMEC,
     *      TIMEI, SUBA, SID, FQID, CREAL, CIMAG, CWT, REFAN, GOTANT,
     *      SUMMGM, CNTMGM, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Loop for more
         IF (.NOT.DONE) GO TO 100
C                                       Close uvdata
         CALL NXTAVG ('CLOS', UVNAME, SOLINT, AVGIF, AVGPOL, ANTWT,
     *      UVRANG, WTUV, MAXAN, NIF, NPOLN, TIMEC, TIMEI, SID, FQID,
     *      BVIS, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Close table
         CALL OTABSN (SNNAME, 'CLOS', SNROW, NP, TIMEC, TIMEI, SID,
     *      IANT, SUBA, FQID, IFR, NODENO, MBDELY, DISP, DDISP, CREAL,
     *      CIMAG, CDELY, CRATE, CWT, REFAN, IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Open table object
         CALL TABOPN (SNNAME, 'WRIT', IERR)
         IF (IERR.GT.0) GO TO 995
C                                       Set sort, time, ant if suba 1
C                                       else unsorted
         IF (SUBA.EQ.1) THEN
            CALL TABCOL (SNNAME, 2, ATCOLS, COLS, IERR)
            IF (IERR.GT.0) GO TO 995
         ELSE
            COLS(1) = 0
            COLS(2) = 0
            END IF
         DIM(1) = 2
         DIM(2) = 1
         CALL TABPUT (SNNAME, 'SORT', OOAINT, DIM, COLS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Update mean gain modulus if
C                                       requested.
C                                       Only if amplitude soln done.
         IF (DOMGM .AND. (CNTMGM.GT.0) .AND.
     *      ((LMODE.EQ.0) .OR. (LMODE.EQ.3))) THEN
            GMMOD = SUMMGM / CNTMGM
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
            CALL TABPUT (SNNAME, 'KEY.MGMOD', OOARE, DIM, GMMOD, CDUMMY,
     *         IERR)
            IF (IERR.GT.0) GO TO 995
            END IF
C                                       Close table object
         CALL TABCLO (SNNAME, IERR)
         IF (IERR.GT.0) GO TO 995
      GO TO 999
C                                       Error
 990  MSGSUP = MSGSAV
      CALL MSGWRT (7)
 995  MSGSUP = MSGSAV
      MSGTXT = 'SLFCAL: ERROR CALIBRATING ' // UVNAME
      CALL MSGWRT (7)
C
 999  MSGSUP = MSGSAV
      RETURN
C-----------------------------------------------------------------------
 2001 FORMAT (' Time=',I4,'/',3I3)
      END
      SUBROUTINE SLFREF (SNNAME, ISUBA, REFANT, IERR)
C-----------------------------------------------------------------------
C   References the phases to a common reference antenna in a
C   polarization coherent fashion.
C   Leaves the output table sorted in time-antenna order.
C   Inputs:
C      SNNAME  C*?  SN table object
C      ISUBA   I    Desired subarray, 0=> 1
C   Input/Output:
C      REFANT  I    Reference antenna, if 0 then the most commonly used
C                   reference antenna is picked.
C   Output:
C      IERR    I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER SNNAME*(*)
      INTEGER   REFANT, ISUBA, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(2)*24, TTYPE*2
      INTEGER   ANT, IIF, ANTUSE(MAXANT), CREFA, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RE1KOL, RE2KOL, IM1KOL, IM2KOL, WT1KOL, WT2KOL,
     *   RF1KOL, RF2KOL, NUMANT, NUMPOL, NUMIF, NUMNOD
      INTEGER DISKIN, CNOIN, SNVER, CATBLK(256), ICLUN, ISNRNO,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), BUFNO, TYPE, DIM(7), ISUB,
     *   KOLRF2, KOLRE2, KOLIM2, KOLWT2
      REAL      GMMOD, RANOD(25), DECNOD(25)
      LOGICAL   ISAPPL
      INTEGER   MXTIME
C                                       MXTIME = dim work arrays
      INCLUDE 'UVSTUFF.INC'
      PARAMETER (MXTIME = BIGVIS/6)
      REAL      WRKS(MXTIME,6)
      EQUIVALENCE (BVIS, WRKS)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      DATA COLHED /'TIME                    ',
     *   'ANTENNA NO.             '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Subarray
      ISUB = MAX (1, ISUBA)
C                                       Must be time antenna order
      CALL TBLSRT (SNNAME, COLHED(1), COLHED(2), IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Lapse into standard AIPSish
C                                       Open object for buffer
      CALL TABOPN (SNNAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Look up table info
      CALL TBLKUP (SNNAME, DISKIN, CNOIN, TTYPE, SNVER, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get CATBLK
      CALL OBHGET (SNNAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get buffer number
      CALL OBINFO (SNNAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get descriptive info
      CALL TABGET (SNNAME, 'KEY.NO_IF', TYPE, DIM, NUMIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TABGET (SNNAME, 'KEY.NO_POL', TYPE, DIM, NUMPOL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TABGET (SNNAME, 'KEY.NO_ANT', TYPE, DIM, NUMANT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Close table; will use buffer
C                                       allocated for the table when it
C                                       was open.
      CALL TABCLO (SNNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Allocate LUN
      CALL OBLUN (ICLUN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       open write
      CALL SNINI ('WRIT', OBUFFR(1,BUFNO), DISKIN, CNOIN, SNVER, CATBLK,
     *   ICLUN, ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get column pointers
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      SOUKOL = SNKOLS(SNISID)
      FRQKOL = SNKOLS(SNIFQI)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      WT1KOL = SNKOLS(SNRWE1)
      RF1KOL = SNKOLS(SNIRF1)
      IF (NUMPOL.GT.1) THEN
         RE2KOL = SNKOLS(SNRRE2)
         IM2KOL = SNKOLS(SNRIM2)
         WT2KOL = SNKOLS(SNRWE2)
         RF2KOL = SNKOLS(SNIRF2)
      ELSE
         RE2KOL = -1
         IM2KOL = -1
         WT2KOL = -1
         RF2KOL = -1
         END IF
C                                       Determine which antennas used as
C                                       reference antennas.
      CALL REFCNT (ISUB, NUMIF, NUMANT, SUBKOL, WT1KOL, WT2KOL, RF1KOL,
     *   RF2KOL, OBUFFR(1,BUFNO), ANTUSE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine reference antenna if
C                                       necessary.
      IF (REFANT.LE.0) THEN
         REFANT = 1
         CREFA = ANTUSE(REFANT)
         DO 30 ANT = 2,NUMANT
            IF (ANTUSE(ANT) .GT. CREFA) THEN
               REFANT = ANT
               CREFA = ANTUSE(REFANT)
               END IF
 30         CONTINUE
         END IF
C                                       Message about rereferencing.
      WRITE (MSGTXT, 1030) REFANT
      CALL MSGWRT (4)
C                                       Loop through antennas used as
C                                       secondary reference antennas.
      DO 500 ANT = 1,NUMANT
         IF ((ANTUSE(ANT).LE.0) .OR. (ANT.EQ.REFANT)) GO TO 500
         DO 300 IIF = 1,NUMIF
C                                       Set column pointers for second
C                                       poln.
            IF (RF2KOL.GT.0) THEN
               KOLRF2 = RF2KOL + IIF - 1
            ELSE
               KOLRF2 = RF2KOL
               END IF
            IF (RE2KOL.GT.0) THEN
               KOLRE2 = RE2KOL + IIF - 1
            ELSE
               KOLRE2 = RE2KOL
               END IF
            IF (IM2KOL.GT.0) THEN
               KOLIM2 = IM2KOL + IIF - 1
            ELSE
               KOLIM2 = IM2KOL
               END IF
            IF (WT2KOL.GT.0) THEN
               KOLWT2 = WT2KOL + IIF - 1
            ELSE
               KOLWT2 = WT2KOL
               END IF
            CALL REFFAZ (ISUB, REFANT, ANT, TIMKOL, SUBKOL, ANTKOL,
     *         RF1KOL+IIF-1, RE1KOL+IIF-1, IM1KOL+IIF-1, WT1KOL+IIF-1,
     *         KOLRF2, KOLRE2, KOLIM2, KOLWT2, OBUFFR(1,BUFNO), MXTIME,
     *         WRKS(1,1), WRKS(1,2), WRKS(1,3), WRKS(1,4), WRKS(1,5),
     *         WRKS(1,6), IERR)
            IF (IERR.NE.0) GO TO 999
 300        CONTINUE
C                                       End of antenna loop
 500     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, OBUFFR(1,BUFNO), OBUFFR(1,BUFNO),
     *   IERR)
      IF ((IERR.NE.0) .OR. (IERR.NE.0)) GO TO 900
C                                       Remove LUN from reserved list
      CALL OBLUFR (ICLUN)
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR, ANT, REFANT
      CALL MSGWRT (8)
 995  MSGTXT = 'SLFREF: ERROR REREFERENCING ' // SNNAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('SLFREF: rereferencing phases to antenna ',I3)
 1900 FORMAT ('SLFREF: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE SLFSMO (UVNAME, SNNAME, ISUBA, IERR)
C-----------------------------------------------------------------------
C   Smooths the SN table which should already be referenced to a single
C   reference antenna.  Failed solutions are interpolated.
C   Leaves the output table sorted in antenna-time order.
C   SN table must be limited to 1 source and FQid or allow smoothing
C   between them.
C   Inputs:
C      UVNAME  C*?  UV DATA object must contain SMOTYPE, SMOAMP,
C                   SMOPHASE keywords (1st = 'MWF', "GAUS' else BOX,
C                   2nd and 3rd smooth times in minutes, 0 => fix failed
C                   only, < 0 => do not do.)
C      SNNAME  C*?  SN table object
C      ISUBA   I    Desired subarray, 0=> 1
C   Output:
C      IERR    I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER UVNAME*(*), SNNAME*(*)
      INTEGER   ISUBA, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER COLHED(2)*24, TTYPE*2, SMTYPE*4, CDUMMY*1, KEYWRD*8
      INTEGER   IIF, TIMKOL, SUBKOL, ANTKOL, RE1KOL,
     *   RE2KOL, IM1KOL, IM2KOL, WT1KOL, WT2KOL, RF1KOL, RF2KOL, NUMANT,
     *   NUMPOL, NUMIF, NUMNOD, DISKIN, CNOIN, SNVER, J, CATBLK(256),
     *   ICLUN, ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), BUFNO, TYPE,
     *   DIM(7), ISUB, DUMMY, MB1KOL, DE1KOL, RA1KOL, MB2KOL, DE2KOL,
     *   RA2KOL
      REAL      GMMOD, RANOD(25), DECNOD(25), SMTAMP, SMTPHS, GNCNT,
     *   GNSUM
      LOGICAL   ISAPPL
      INTEGER   MXTIME
C                                       MXTIME = dim work arrays
      INCLUDE 'UVSTUFF.INC'
      PARAMETER (MXTIME = (BIGVIS+3*MAXCIF)/10)
      REAL      WRKS(MXTIME,10)
      EQUIVALENCE (VS(1,1), WRKS)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      DATA COLHED /'TIME', 'ANTENNA NO.'/
      DATA KEYWRD /'MGMOD'/
C-----------------------------------------------------------------------
      CALL OGET (UVNAME, 'SMOTYPE', TYPE, DIM, DUMMY, SMTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (UVNAME, 'SMOAMP', TYPE, DIM, SMTAMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (UVNAME, 'SMOPHASE', TYPE, DIM, SMTPHS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF ((SMTPHS.LT.0.0) .AND. (SMTAMP.LT.0.0)) GO TO 999
      SMTAMP = SMTAMP / 1440.0
      SMTPHS = SMTPHS / 1440.0
C                                       Subarray
      ISUB = MAX (1, ISUBA)
C                                       Must be antenna time order
      CALL TBLSRT (SNNAME, COLHED(2), COLHED(1), IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Lapse into standard AIPSish
C                                       Open object for buffer
      CALL TABOPN (SNNAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Look up table info
      CALL TBLKUP (SNNAME, DISKIN, CNOIN, TTYPE, SNVER, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get CATBLK
      CALL OBHGET (SNNAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get buffer number
      CALL OBINFO (SNNAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get descriptive info
      CALL TABGET (SNNAME, 'KEY.NO_IF', TYPE, DIM, NUMIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TABGET (SNNAME, 'KEY.NO_POL', TYPE, DIM, NUMPOL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TABGET (SNNAME, 'KEY.NO_ANT', TYPE, DIM, NUMANT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Close table; will use buffer
C                                       allocated for the table when it
C                                       was open.
      CALL TABCLO (SNNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Allocate LUN
      CALL OBLUN (ICLUN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       open write
      CALL SNINI ('WRIT', OBUFFR(1,BUFNO), DISKIN, CNOIN, SNVER, CATBLK,
     *   ICLUN, ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get column pointers
      GNCNT = 0.0
      GNSUM = 0.0
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      MB1KOL = SNKOLS(SNRMD1)
      MB2KOL = SNKOLS(SNRMD2)
      DO 300 IIF = 1,NUMIF
         J = IIF - 1
         RE1KOL = SNKOLS(SNRRE1) + J
         IM1KOL = SNKOLS(SNRIM1) + J
         WT1KOL = SNKOLS(SNRWE1) + J
         RF1KOL = SNKOLS(SNIRF1) + J
         RA1KOL = SNKOLS(SNRRA1) + J
         DE1KOL = SNKOLS(SNRDE1) + J
         IF (NUMPOL.GT.1) THEN
            RE2KOL = SNKOLS(SNRRE2) + J
            IM2KOL = SNKOLS(SNRIM2) + J
            WT2KOL = SNKOLS(SNRWE2) + J
            RF2KOL = SNKOLS(SNIRF2) + J
            RA2KOL = SNKOLS(SNRRA2) + J
            DE2KOL = SNKOLS(SNRDE2) + J
         ELSE
            RE2KOL = -1
            IM2KOL = -1
            WT2KOL = -1
            RF2KOL = -1
            RA2KOL = -1
            DE2KOL = -1
            END IF
         CALL SNSMOO (SMTYPE, 0.5, SMTAMP, SMTPHS, ISUB, NUMANT, TIMKOL,
     *      SUBKOL, ANTKOL, RF1KOL, MB1KOL, DE1KOL, RA1KOL, RE1KOL,
     *      IM1KOL, WT1KOL, RF2KOL, MB2KOL, DE2KOL, RA2KOL, RE2KOL,
     *      IM2KOL, WT2KOL, OBUFFR(1,BUFNO), GNCNT, GNSUM,
     *      OBUFFR(1025,BUFNO), MXTIME, WRKS, IERR)
         IF (IERR.NE.0) GO TO 999
 300     CONTINUE
C                                       Update GMMOD
      IF ((ABS (GMMOD-1.0).GT.1.0E-5) .AND. (GNCNT.GT.0.1)) THEN
         GMMOD = GNSUM / GNCNT
         CALL TABKEY ('WRIT', KEYWRD, 1, OBUFFR(1,BUFNO), 1, GMMOD, 2,
     *      IERR)
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, OBUFFR(1,BUFNO), OBUFFR(1,BUFNO),
     *   IERR)
      IF ((IERR.NE.0) .OR. (IERR.NE.0)) GO TO 900
C                                       Remove LUN from reserved list
      CALL OBLUFR (ICLUN)
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR, SNVER
      CALL MSGWRT (8)
 995  MSGTXT = 'SLFSMO: ERROR SMOOTHING ' // SNNAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SLFSMO: TABIO ERROR',I3,' SMOOTHING SN TABLE',I5)
      END
      SUBROUTINE SLFPA (VOBS, MAXAN, MAXIFS, MAXPOL, NUMIF, REFANT,
     *   AVGIF, AVGPOL, DOL1, DOGCON, GAERR, MODE, MINNO, SOLCON,
     *   SNRMIN, PRTLV, FFLIM, FFLAST, CREAL, CIMAG, CWT, REFAN, GOTANT)
C-----------------------------------------------------------------------
C   Private routine to selfcal system
C   SLBPA does least squares solutions for phase and optionally
C   amplitude.  Three methods are available, "normal", "L1" and
C   "amplitude constrained".
C      All frequencies in each IF and at all times are assumed to have
C   been averaged.  If AVGIF is true then the data is assumed to have
C   been averaged in frequency and the solution found for the first IF
C   is copied to all IFs.
C   If AVGPOL  is true then the data is assumed to have been averaged in
C   polarization  and the data is copied to the second
C   Input:
C      VOBS       R(2,maxif,maxpol,maxant,maxant)
C                     First entry (1,if,pol,?,?):
C                     Real part in upper half (1,if,pol,i,j) i<j.
C                     Imaginary part in lower half.
C                     Second entry (2,if,pol,?,?):
C                     Weight in upper part, count in lower.
C      MAXAN       I    Maximum antenna number present in VOBS
C      MAXIFS      I    Maximum number of IFs present in VOBS
C      MAXPOL      I    Maximum number of Stokes' in VOBS
C      NUMIF       I    Number of IFs
C      REFANT      I    Reference antenna to use.
C      AVGIF       L    If true average IFs.
C      AVGPOL      L    If true average polarizations.
C      DOL1        L    If true, use L1 solution.
C      DOGCON      L    If true, use constrained soln
C      GAERR(*)    R    The estimated variance of the antenna
C                       amplitudes; 1 per antenna.
C      MODE        I    Solution mode; 0= full gain, 1=phase
C                       2=phase(ignore amp), 3=full, constrain
C                       amplitude, 4-7 as 0-3 with robust methods
C      MINNO       I    Minimum number of antannas allowed
C      SOLCON      R    Factor for amp. penalty term.
C      SNRMIN      R    Minimum SNR allowed.
C      PRTLV       I    Print level, .ge. 2 gives some print.
C      FFLIM       R    Limit clip to max (std, FFLIM) * rms
C      FFLAST      R    If data closure > FFLAST * rms closure then
C                       weights marked as bad - not now used for any
C                       real purpose except in CALIB
C   Output:
C      CREAL       R(2,MAXIF,NUMANT) Real part of solution
C      CIMAG       R(2,MAXIF,NUMANT) Imag part of solution
C      CWT         R(2,MAXIF,NUMANT) Weights = SNR
C      REFAN       I(2,MAXIF)        Reference antennas used
C      GOTANT      L(*)              If true corresponding antenna has
C                                    data.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXAN, MAXIFS, MAXPOL, NUMIF, REFANT, MODE, MINNO,
     *   PRTLV, REFAN(2,MAXIF)
      LOGICAL   AVGIF, AVGPOL, DOL1, DOGCON, GOTANT(*)
      REAL      VOBS(2,MAXIFS,MAXPOL,MAXAN, MAXAN),
     *   CREAL(2,MAXIF,*), CIMAG(2,MAXIF,*), CWT(2,MAXIF,*), GAERR(*),
     *   SNRMIN, SOLCON, FFLIM, FFLAST
C
      INTEGER   IREF, IIF, IA1, IA2, IST, IANT, IERR, LWT, LMODE, LBL
      INTEGER   NBL, IIS(MXBASE), JJS(MXBASE)
      REAL      GAIN(2,MAXANT), SNR(MAXANT), WTBT(MXBASE), TIME, AMP,
     *   CLOSER(2,2), FRAC, RMS, TEMP, XXX
      CHARACTER PRTSOU*16
      LOGICAL   INIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:GAIN.INC'
C                                       No printout from CLBSNR
      DATA TIME, CLOSER /0.0, 4*1.E10/
      DATA PRTSOU /'    '/
C-----------------------------------------------------------------------
C                                       interpret mode
      LWT = MODE / 10
      LMODE = MOD (MODE, 10)
      IF (LMODE.GT.3) LMODE = LMODE - 4
      INIT = .TRUE.
      DO 10 IANT = 1,MAXAN
         GOTANT(IANT) = .FALSE.
 10      CONTINUE
C                                       (Loop) over Stokes' type
      DO 600 IST = 1,MAXPOL
C                                       Loop over IF
         DO 500 IIF = 1,MAXIFS
C                                       Copy data to XOBS
            NBL = 0
            DO 50 IA1 = 1,MAXAN-1
               DO 40 IA2 = IA1+1,MAXAN
                  IF (VOBS(2,IIF,IST,IA1,IA2).GT.1.0E-20) THEN
                     NBL = NBL + 1
                     XOBS(1,NBL) = VOBS(1,IIF,IST,IA1,IA2)
                     XOBS(2,NBL) = VOBS(1,IIF,IST,IA2,IA1)
                     WTBT(NBL) = VOBS(2,IIF,IST,IA1,IA2)
                     IIS(NBL) = IA1
                     JJS(NBL) = IA2
                     GOTANT(IA1) = .TRUE.
                     GOTANT(IA2) = .TRUE.
                     END IF
 40               CONTINUE
 50            CONTINUE
C                                       Blank output
            DO 60 IANT = 1,MAXAN
               CREAL(IST,IIF,IANT) = FBLANK
               CIMAG(IST,IIF,IANT) = FBLANK
               CWT(IST,IIF,IANT) = 0.0
 60            CONTINUE
            REFAN(IST,IIF) = 0
            IREF = REFANT
C                                       Do solution
C                                       Amplitude constrained
            IF (DOGCON) THEN
               CALL  NCALC (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *            MODE, MINNO, GAERR, SOLCON, GAIN, IREF, PRTLV, FFLIM,
     *            FFLAST, FRAC, RMS, IERR)
C                                       L1 solution
            ELSE IF (DOL1) THEN
               CALL GCALC1 (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *            MODE, MINNO, GAIN, IREF, PRTLV, FFLIM, FFLAST, FRAC,
     *            RMS, IERR)
C                                       Normal
            ELSE
               CALL GCALC (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *            MODE, MINNO, GAIN, IREF, HESS, PRTLV, FFLIM, FFLAST,
     *            FRAC, RMS, IERR)
               END IF
C                                       restore true weights
            LBL = 0
            DO 80 IA1 = 1,MAXAN-1
               DO 70 IA2 = IA1+1,MAXAN
                  IF (VOBS(2,IIF,IST,IA1,IA2).GT.1.0E-20) THEN
                     LBL = LBL + 1
                     IF (WTBT(LBL).LE.0.0) THEN
                        VOBS(2,IIF,IST,IA1,IA2) = WTBT(LBL)
                     ELSE IF (LWT.EQ.1) THEN
                        WTBT(LBL) = WTBT(LBL) ** 2
                     ELSE IF (LWT.EQ.2) THEN
                        WTBT(LBL) = WTBT(LBL) ** 4
                     ELSE IF (LWT.EQ.3) THEN
                        WTBT(LBL) = VOBS(2,IIF,IST,IA1,IA2)
                        IF (LMODE.EQ.2) THEN
                           XXX = SQRT (XOBS(1,LBL)**2 + XOBS(2,LBL)**2)
                           WTBT(LBL) = WTBT(LBL) * XXX * XXX
                           END IF
                        END IF
                     END IF
 70               CONTINUE
 80            CONTINUE
C                                       Solution failed
            IF (IERR.NE.0) THEN
               TEMP = -1.0
               IF (IERR.EQ.1) TEMP = -2.0
               DO 100 IANT = 1,MAXAN
                  CWT(IST,IIF,IANT) = TEMP
 100              CONTINUE
               GO TO 500
               END IF
C                                       Convert amplitude to 1/amp
C                                       to correct data.
            DO 110 IANT = 1,MAXAN
               AMP = GAIN(1,IANT)*GAIN(1,IANT) +
     *               GAIN(2,IANT)*GAIN(2,IANT)
               IF (AMP.LT.1.0E-20) AMP = 1.0
               GAIN(1,IANT) = GAIN(1,IANT) / AMP
               GAIN(2,IANT) = GAIN(2,IANT) / AMP
 110           CONTINUE
C                                       Compute SNRs
            IF (PRTLV.GT.0) PRTLV = PRTLV + 3
            CALL CLBSNR (XOBS, IIS, JJS, WTBT, NBL, MAXAN, GAIN, SNR,
     *         CLOSER, SNRMIN, TIME, IIF, IST, GWORK, PRTLV, 2.5,
     *         PRTSOU)
            IF (PRTLV.GT.4) PRTLV = PRTLV - 3
C                                       Save results
            DO 150 IANT = 1,MAXAN
               IF (SNR(IANT).GT.SNRMIN) THEN
                  CREAL(IST,IIF,IANT) = GAIN(1,IANT)
                  CIMAG(IST,IIF,IANT) = GAIN(2,IANT)
                  CWT(IST,IIF,IANT) = SNR(IANT)
                  END IF
 150           CONTINUE
            REFAN(IST,IIF) = IREF
C                                       Averaging in poln?
            IF (AVGPOL) THEN
               DO 200 IANT = 1,MAXAN
                  CREAL(2,IIF,IANT) = CREAL(1,IIF,IANT)
                  CIMAG(2,IIF,IANT) = CIMAG(1,IIF,IANT)
                  CWT(2,IIF,IANT) = CWT(1,IIF,IANT)
 200              CONTINUE
               REFAN(2,IIF) = REFAN(1,IIF)
               END IF
 500        CONTINUE
 600     CONTINUE
C                                       If averaging in IF copy soln.
      IF (AVGIF) THEN
         DO 620 IIF = 2,NUMIF
            DO 610 IANT = 1,MAXAN
               CREAL(1,IIF,IANT) = CREAL(1,1,IANT)
               CIMAG(1,IIF,IANT) = CIMAG(1,1,IANT)
               CWT(1,IIF,IANT) = CWT(1,1,IANT)
               CREAL(2,IIF,IANT) = CREAL(2,1,IANT)
               CIMAG(2,IIF,IANT) = CIMAG(2,1,IANT)
               CWT(2,IIF,IANT) = CWT(2,1,IANT)
 610        CONTINUE
            REFAN(1,IIF) = REFAN(1,1)
            REFAN(2,IIF) = REFAN(2,1)
 620        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SLFSOU (SNNAME, SNROW, NUMAN, NUMIF, NUMPOL, TIMEC,
     *   TIMEI, SUBA, SID, FQID, CREAL, CIMAG, CWT, REFAN, GOTANT,
     *   SUMMGM, CNTMGM, IERR)
C-----------------------------------------------------------------------
C   Private routine to selfcal system
C   SLFSOU prepares a set of self calibration SN table entries and
C   writes them to an SN table Object.  If the weight of a solution is
C   0.0 it is assumed that there was insufficient data for the solution.
C   SLFPA sets weights to -1 if the solution fails.
C   The SN table object should be opened before the first call and
C   closed after the last call.
C      Also keeps statistics for determining mean gain modulus.
C   Input:
C      SNNAME   C*?   SN table object
C      NUMAN    I     Maximum antenna number.
C      NUMIF    I     Number of IFs
C      NUMPOL   I     Number of polarizations.
C      TIMEC    D     Time in days
C      TIMEI    R     Solution interval in days
C      SUBA     I     Subarray number
C      SID      I     Source number
C      FQID     I     FQ id
C      CREAL    R(2,MAXIF,NUMANT) Real part of solution
C      CIMAG    R(2,MAXIF,NUMANT) Imag part of solution
C      CWT      R(2,MAXIF,NUMANT) Weights = SNR
C      REFAN    I(2,MAXIF) Reference antennas used
C      GOTANT   L(*)  Flags indicating if there was data for each ant.
C   Input/Output:
C      SNROW    I     SN row number
C      SUMMGM   D     Sum of gain modulus
C      CNTMGM   I     Count of contributions to SUMMGM
C   Output:
C      IERR     I     Return code, 0=>OK, else TABSN error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SNNAME*(*)
      INTEGER   SNROW, NUMAN, NUMIF, NUMPOL, SUBA, SID, FQID,
     *   REFAN(2,*), CNTMGM, IERR
      LOGICAL   GOTANT(*)
      DOUBLE PRECISION TIMEC, SUMMGM
      REAL      TIMEI, CREAL(2,MAXIF,*), CIMAG(2,MAXIF,*),
     *   CWT(2,MAXIF,*)
C
      INTEGER   NODENO, IANT, IIF
      REAL      MBDELY(2), CDELY(2,MAXIF), CRATE(2,MAXIF), IFR, DISP(2),
     *   DDISP(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NODENO /1/
      DATA MBDELY, DISP, DDISP /6*0.0/
      DATA CDELY /MAXIF*0.0, MAXIF*0.0/
      DATA CRATE /MAXIF*0.0, MAXIF*0.0/
      DATA IFR /0.0/
C-----------------------------------------------------------------------
C                                       Loop over antennae
      DO 420 IANT = 1,NUMAN
C                                       Any data?
         IF (GOTANT(IANT)) THEN
            DO 410 IIF = 1,NUMIF
C                                       Sum mean gain modulus for first
C                                       poln.
               IF ((CWT(1,IIF,IANT).GT.0.0) .AND.
     *            (CREAL(1,IIF,IANT).NE.FBLANK) .AND.
     *            (CIMAG(1,IIF,IANT).NE.FBLANK)) THEN
                  CNTMGM = CNTMGM + 1
                  SUMMGM = SUMMGM + SQRT (CREAL(1,IIF,IANT)**2 +
     *               CIMAG(1,IIF,IANT)**2)
                  END IF
C                                       Sum mean gain modulus for second
C                                       poln.
               IF ((NUMPOL.GT.1) .AND. (CWT(2,IIF,IANT).GT.0.0) .AND.
     *            (CREAL(2,IIF,IANT).NE.FBLANK) .AND.
     *            (CIMAG(2,IIF,IANT).NE.FBLANK)) THEN
                  CNTMGM = CNTMGM + 1
                  SUMMGM = SUMMGM + SQRT (CREAL(2,IIF,IANT)**2 +
     *               CIMAG(2,IIF,IANT)**2)
                  END IF
 410           CONTINUE
            CALL OTABSN (SNNAME, 'WRIT', SNROW, NUMPOL, TIMEC, TIMEI,
     *         SID, IANT, SUBA, FQID, IFR, NODENO, MBDELY, DISP, DDISP,
     *         CREAL(1,1,IANT), CIMAG(1,1,IANT), CDELY, CRATE,
     *         CWT(1,1,IANT), REFAN, IERR)
            IF (IERR.GT.0) GO TO 995
            END IF
 420     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'SLFSOU: ERROR WRITING SOLUTIONS FOR ' // SNNAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE FRSOLN (VOBS, MAXAN, MAXIFS, MAXPOL, NUMIF, AVGIF,
     *   SNRMIN, RATMIN, RATMAX, PRTLV, CREAL, CIMAG, CWT, REFAN,
     *   GOTANT)
C-----------------------------------------------------------------------
C   Private routine to Faradey rotation selfcal system
C   SLBPA does least squares solutions for phase and optionally
C   amplitude.  Three methods are available, "normal", "L1" and
C   "amplitude constrained".
C      All frequencies in each IF and at all times are assumed to have
C   been averaged.  If AVGIF is true then the data is assumed to have
C   been averaged in frequency and the solution found for the first IF
C   is copied to all IFs.
C   If AVGPOL  is true then the data is assumed to have been averaged in
C   polarization  and the data is copied to the second
C   Input:
C      VOBS       R(2,maxif,maxpol,maxant,maxant)
C                     First entry (1,if,pol,?,?):
C                     Real part in upper half (1,if,pol,i,j) i<j.
C                     Imaginary part in lower half.
C                     Second entry (2,if,pol,?,?):
C                     Weight in upper part, count in lower.
C      MAXAN       I    Maximum antenna number present in VOBS
C      MAXIFS      I    Maximum number of IFs present in VOBS
C      MAXPOL      I    Maximum number of Stokes' in VOBS
C      NUMIF       I    Number of IFs
C      AVGIF       L    If true average IFs.
C      SNRMIN      R    Minimum SNR allowed.
C      RATMIN      R    Minimum amplitude.
C      RATMAX      R    Maximum amplitude.
C      PRTLV       I    Print level
C   Output:
C      CREAL       R(2,MAXIF,NUMANT) Real part of solution
C      CIMAG       R(2,MAXIF,NUMANT) Imag part of solution
C      CWT         R(2,MAXIF,NUMANT) Weights = SNR
C      REFAN       I(2,MAXIF)        Reference antennas used (dummmy)
C      GOTANT      L(*)              If true corresponding antenna has
C                                    data.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXAN, MAXIFS, MAXPOL, NUMIF, PRTLV, REFAN(2,MAXIF)
      LOGICAL   AVGIF, GOTANT(*)
      REAL      VOBS(2,MAXIFS,MAXPOL,MAXAN, MAXAN),
     *   CREAL(2,MAXIF,*), CIMAG(2,MAXIF,*), CWT(2,MAXIF,*), SNRMIN,
     *   RATMIN, RATMAX
C
      INTEGER   IIF, IA1, IA2, IANT, REFUSE, COUNT
      REAL      SNR, WT, AMP2, RMAX2, RMIN2, SUMRE, SUMIM, SUMWT,
     *   SUMRE2, SUMIM2, PHAS, REPHAS, IMPHAS, VI, VR, VP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE REFUSE
      DATA REFUSE /-1/
C-----------------------------------------------------------------------
      DO 10 IANT = 1,MAXAN
         GOTANT(IANT) = .FALSE.
 10      CONTINUE
      RMAX2 = RATMAX * RATMAX
      RMIN2 = RATMIN * RATMIN
C                                       Loop over IF
      DO 500 IIF = 1,MAXIFS
C                                       Check data and average
         SUMRE = 0.0
         SUMIM = 0.0
         SUMRE2 = 0.0
         SUMIM2 = 0.0
         SUMWT = 0.0
         COUNT = 0
         DO 50 IA1 = 1,MAXAN-1
            DO 40 IA2 = IA1+1,MAXAN
C                                       RL
               IF (VOBS(2,IIF,3,IA1,IA2).GT.1.0E-20) THEN
C                                       Acceptable data?
                  AMP2 = VOBS(1,IIF,3,IA1,IA2)*VOBS(1,IIF,3,IA1,IA2) +
     *               VOBS(1,IIF,3,IA2,IA1)*VOBS(1,IIF,3,IA2,IA1)
                  IF ((AMP2.LT.RMIN2) .OR. (AMP2.GT.RMAX2)) THEN
                     VOBS(2,IIF,3,IA1,IA2) = 0.0
                     GO TO 30
                     END IF
                  GOTANT(IA1) = .TRUE.
                  GOTANT(IA2) = .TRUE.
                  IF (REFUSE.LT.0) REFUSE = IA1
C                                       Sum
                  WT = MAX (VOBS(2,IIF,3,IA1,IA2), 0.0)
                  SUMRE = SUMRE + VOBS(1,IIF,3,IA1,IA2) * WT
                  SUMIM = SUMIM + VOBS(1,IIF,3,IA2,IA1) * WT
                  SUMRE2 = SUMRE2 + (VOBS(1,IIF,3,IA1,IA2)**2) * WT
                  SUMIM2 = SUMIM2 + (VOBS(1,IIF,3,IA2,IA1)**2) * WT
                  SUMWT = SUMWT + WT
                  COUNT = COUNT + 1
                  END IF
C                                       LR
 30            IF (VOBS(2,IIF,4,IA1,IA2).GT.1.0E-20) THEN
C                                       Acceptable data?
                  AMP2 = VOBS(1,IIF,4,IA1,IA2)*VOBS(1,IIF,4,IA1,IA2) +
     *               VOBS(1,IIF,4,IA2,IA1)*VOBS(1,IIF,4,IA2,IA1)
                  IF ((AMP2.LT.RMIN2) .OR. (AMP2.GT.RMAX2)) THEN
                     VOBS(2,IIF,4,IA1,IA2) = 0.0
                     GO TO 40
                     END IF
                  GOTANT(IA1) = .TRUE.
                  GOTANT(IA2) = .TRUE.
                  IF (REFUSE.LT.0) REFUSE = IA1
C                                       Sum conjugate
                  WT = MAX (VOBS(2,IIF,4,IA1,IA2), 0.0)
                  SUMRE = SUMRE + VOBS(1,IIF,4,IA1,IA2) * WT
                  SUMIM = SUMIM - VOBS(1,IIF,4,IA2,IA1) * WT
                  SUMRE2 = SUMRE2 + (VOBS(1,IIF,4,IA1,IA2)**2) * WT
                  SUMIM2 = SUMIM2 + (VOBS(1,IIF,4,IA2,IA1)**2) * WT
                  SUMWT = SUMWT + WT
                  COUNT = COUNT + 1
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       Determine SNR
         IF ((SUMWT.GT.0.0) .AND. (COUNT.GE.3)) THEN
C                                       Variance of real (mean)
            VR = ((SUMRE2 - ((SUMRE*SUMRE) / SUMWT)) / (SUMWT*COUNT))
C                                       Variance of imaginary
            VI = ((SUMIM2 - ((SUMIM*SUMIM) / SUMWT)) / (SUMWT*COUNT))
C                                       Variance of phase
            VP = ((SUMRE/SUMWT)**2) * VI + ((SUMIM/SUMWT)**2) * VR
            IF (VP.GT.1.0E-20) THEN
               SNR = 1.0 / SQRT (VP)
            ELSE
               SNR = 0.0
               END IF
         ELSE
            SNR = 0.0
            END IF
C                                       Average phase
         IF ((SUMWT.GT.0) .AND. (SNR.GE.SNRMIN)) THEN
            SUMRE = SUMRE / SUMWT
            SUMIM = SUMIM / SUMWT
            PHAS =  ATAN2 (SUMIM, SUMRE+1.0E-20)
            REPHAS = COS (PHAS)
            IMPHAS = SIN (PHAS)
C                                       Subtract from L
            DO 60 IANT = 1,MAXAN
               CREAL(1,IIF,IANT) = 1.0
               CIMAG(1,IIF,IANT) = 0.0
               CWT(1,IIF,IANT) = SNR
               CREAL(2,IIF,IANT) = REPHAS
               CIMAG(2,IIF,IANT) = -IMPHAS
               CWT(2,IIF,IANT) = SNR
 60            CONTINUE
            REFAN(1,IIF) = REFUSE
            REFAN(2,IIF) = REFUSE
         ELSE
C                                       Bad - Blank output
            DO 70 IANT = 1,MAXAN
               CREAL(1,IIF,IANT) = FBLANK
               CIMAG(1,IIF,IANT) = FBLANK
               CWT(1,IIF,IANT) = 0.0
               CREAL(2,IIF,IANT) = FBLANK
               CIMAG(2,IIF,IANT) = FBLANK
               CWT(2,IIF,IANT) = 0.0
 70            CONTINUE
            REFAN(1,IIF) = 0
            REFAN(2,IIF) = 0
            END IF
C                                       Message
            IF (PRTLV.GT.0) THEN
               IF (SNR.GT.SNRMIN) THEN
                  PHAS = PHAS * 57.296
                  WRITE (MSGTXT,1070) IIF, PHAS, SNR
               ELSE
                  WRITE (MSGTXT,1071) IIF
                  END IF
               CALL MSGWRT (4)
               END IF
 500     CONTINUE
C                                       If averaging in IF copy soln.
      IF (AVGIF) THEN
         DO 620 IIF = 2,NUMIF
            DO 610 IANT = 1,MAXAN
               CREAL(1,IIF,IANT) = CREAL(1,1,IANT)
               CIMAG(1,IIF,IANT) = CIMAG(1,1,IANT)
               CWT(1,IIF,IANT) = CWT(1,1,IANT)
               CREAL(2,IIF,IANT) = CREAL(2,1,IANT)
               CIMAG(2,IIF,IANT) = CIMAG(2,1,IANT)
               CWT(2,IIF,IANT) = CWT(2,1,IANT)
 610        CONTINUE
            REFAN(1,IIF) = REFAN(1,1)
            REFAN(2,IIF) = REFAN(2,1)
 620        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('  IF(',I2,') R-L phase = ',F8.2,' deg. SNR = ',F15.3)
 1071 FORMAT ('  IF(',I2,') Solution failed')
      END
      SUBROUTINE CP2SCR (UVDATA, UVSCR, CMPSCR, IERR)
C-----------------------------------------------------------------------
C   Routine to Copy all subarrays to a scratch file
C   Inputs:
C      UVDATA  C*(*)  Input uv object
C      UVSCR   C*(*)  Scratch uv object.
C      CMPSCR  L      Try and compress the scratch file?
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*)
      LOGICAL   CMPSCR
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), ANVER, NSUBA, ISUBA, COUNT
      CHARACTER ANTAB*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Number of subarrays
      ANTAB = 'Temp AN for SCLOOP'
      ANVER = 1
      CALL UV2TAB (UVDATA, ANTAB, 'AN', ANVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, IERR)
      IF (IERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy
      DO 50 ISUBA = 1,NSUBA
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVRSCR (UVDATA, UVSCR, CMPSCR, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set vis offset to append
         CALL OGET (UVSCR, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 50      CONTINUE
C                                       Reset selection on objects
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      ISUBA = 0
      CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVPUT (UVSCR, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'CP2SCR:ERROR COPYING UVDATA'
      CALL MSGWRT (8)
C
 999  RETURN
      END
