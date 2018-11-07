LOCAL INCLUDE 'BASELN.INC'
C
C   A list of requested baselines.
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   NBASLN
C                                       The number of baselines in the
C                                       list.
C                                       1 <= NBASLN
C
      INTEGER   BASLST (2, MXBASE)
C                                       The baseline list: BASLST(1, i)
C                                       is the primary antenna number
C                                       and BASLST(2, i) is the
C                                       secondary antenna number for the
C                                       i-th baseline.
C                                       For any i, 1 <= i <= NBASLN
C                                         1 <= BASLST(1, i)
C                                           <= BASLST(2, i) <= MAXANT
C
      LOGICAL   AEXIST(MAXANT)
C                                       AEXIST(i) is true if and only if
C                                       there is an antenna with ID
C                                       number i in the requested
C                                       subarray.
C
      COMMON /BASELN/ BASLST, NBASLN, AEXIST
      SAVE /BASELN/
LOCAL END
LOCAL INCLUDE 'BSREC.INC'
C
C   BS table record buffer
C
      INCLUDE 'INCS:PUVD.INC'
C
      DOUBLE PRECISION CTIME
C                                       Centre time for the solution
C                                       interval in days from 0h on the
C                                       reference day.
C
      REAL      TIMINT
C                                       Time interval covered by the
C                                       solution in days.
C
      INTEGER   BASELN(2)
C                                       The numbers of the antennae
C                                       defining the baseline.
C
      INTEGER   SUBARR
C                                       The subarray number
C
      INTEGER   STOKES
C                                       The stokes code
C
      INTEGER   SOURCE
C                                       The source ID number
C
      REAL      VAMP(MAXIF)
C                                       The amplitude of the vector sum
C                                       of the post-fringe visibilities
C                                       for each IF in Jy.
C
      REAL      SAMP(MAXIF)
C                                       The sum of the scalar visibility
C                                       amplitudes for each IF over the
C                                       solution interval.
C
      REAL      RMBD, MBDERR, MBDAMB
C                                       The residual multiband delay,
C                                       delay error and ambiguity (sec).
C
      REAL      RSBD(MAXIF), SBDERR(MAXIF), SBDAMB
C                                       The residual single-band delay
C                                       and error for each IF and the
C                                       single-band delay ambiguity
C                                       (sec).
C
      REAL      RRATE(MAXIF), RTERR(MAXIF), RTAMB
C                                       The residual rate and error for
C                                       each IF and the rate ambiguity
C                                       in Hz.
C
      REAL      RACCEL(MAXIF), ACCERR(MAXIF)
C                                       The residual acceleration and
C                                       error for each IF in Hz**2
C
      REAL      RPHASE(MAXIF), PHSERR(MAXIF)
C                                       The residual phase and phase
C                                       error for each IF in degrees.
C
      INTEGER   BSROW
C                                       The next BS table row to write.
C
      INTEGER   BSIF
C                                       The number of IF entries in the
C                                       BS table.
C
      COMMON /BSREC/ CTIME, TIMINT, BASELN, SUBARR, STOKES, SOURCE,
     *   VAMP, SAMP, RMBD, MBDERR, MBDAMB, RSBD, SBDERR, SBDAMB, RRATE,
     *   RTERR, RTAMB, RACCEL, ACCERR, RPHASE, PHSERR, BSROW, BSIF
      SAVE /BSREC/
C
LOCAL END
LOCAL INCLUDE 'CONTROL.INC'
C
C   Task control parameters.
C
      LOGICAL   DIVMOD
C                                       Divide model into data?
      LOGICAL   STACK
C                                       Stack data?
      REAL      CTHRSH
C                                       Coherence threshold
C                                       (0.0 < CTHRSH <= 1.0)
      REAL      MINSNR
C                                       Minimum SNR to accept
C                                       (0.0 < MINSNR)
C
      REAL      MININT
C                                       Minimum integration time in
C                                       data file in days
C                                       (0.0 < MININT)
C
      INTEGER   MODE
C                                       Solution mode
C                                         1  Independent IFs
C                                         2  Single delay for all IFs
C                                         3  Separate multiband dly
C                                         4  rate only
C
      INTEGER   DBLOAT, RBLOAT
C                                       Bloat factors for the FFT
C                                       search (powers of 2)
      COMMON /CONTRL/ DIVMOD, STACK, CTHRSH, MINSNR, MININT, MODE,
     *   DBLOAT, RBLOAT
      SAVE /CONTRL/
LOCAL END
LOCAL INCLUDE 'INPUTS.INC'
C
C   Declarations for the adverb descriptions.
C
C   Each adverb has an upper-case name, up to 8 characters in length,
C   and a two dimensional value array.  The adverb names are listed in
C   AVNAME in the order in which they occur in BLING.HLP, the dimensions
C   of the value array are listed in the correponding elements of AVDIM
C   and the types of the adverb values are listed in the corresponding
C   elements of AVTYPE.
C
      INTEGER   NPARMS
      PARAMETER (NPARMS = 46)
C                                       The number of input adverbs
      INTEGER   AVTYPE(NPARMS)
C                                       The type code for each adverb
      INTEGER   AVDIM(2, NPARMS)
C                                       The array dimensions for each
C                                       adverb value
      CHARACTER AVNAME(NPARMS)*8
C                                       The name of each adverb
      COMMON /INPUTS/ AVTYPE, AVDIM
      COMMON /INPCHR/ AVNAME
      SAVE /INPUTS/, /INPCHR/
C
LOCAL END
LOCAL INCLUDE 'SEARCH.INC'
C                                       Search parameters
C
C A number of arrays are used to store search parameter settings.
C
      INTEGER   MXSRCH
      PARAMETER (MXSRCH = 1000)
C                                       Maximum number of search
C                                       settings.
C
      INTEGER   NMSRCH
C                                       The actual number of search
C                                       settings.
C
      INTEGER   SRCHBL(2, MXSRCH)
C                                       The baseline for which each
C                                       setting is applicable.  The
C                                       two numbers correspond to
C                                       antenna numbers from the
C                                       antennas table with zero being
C                                       used to denote any telescope.
C                                       For any i, 1 <= i <= NMSRCH
C                                         SRCHBL(1, i) >= 0
C                                         SRCHBL(2, i) >= 0
C                                         SRCHBL(2, i) > 0 =>
C                                           SRCHBL(2, i) > SRCHBL(1, i)
C
      REAL      SRCHSI(MXSRCH)
C                                       Solution intervals in days.
C                                       For any i, 1 <= i < NMSRCH
C                                         SRCHSI > 0.0
C
      REAL      SRCHTI(2, MXSRCH)
C                                       Time interval: first element is
C                                       the start time, second is the
C                                       stop time in days.
C                                       For any i, 1 <= i < NMSRCH
C                                         SRCHTI(1, i) >= 0.0
C                                         SRCHTI(2, i) > SRCHTI(1, i)
      REAL      SRCHWN(2, 4, MXSRCH)
C                                       Windows: SRCHWN(i, j, k) is
C                                       the centre (i = 1) or width
C                                       (i = 2) of the multiband delay
C                                       window (j = 1), the single-band
C                                       delay window (j = 2), the rate
C                                       window (j = 3) or the
C                                       acceleration window (j = 4).
C                                       Delay windows are specified in
C                                       seconds, rate windows in Hz and
C                                       acceleration windows in Hz**2.
C
      REAL      SRCHDA(MXSRCH)
C                                       The acceleration search step in
C                                       Hz**2.
C
      COMMON /SEARCH/ NMSRCH, SRCHBL, SRCHSI, SRCHTI, SRCHWN, SRCHDA
      SAVE /SEARCH/
C
LOCAL END
LOCAL INCLUDE 'UVINFO.INC'
C
C Information about the uv data set.
C
      INTEGER   MAXIS
      PARAMETER (MAXIS = 7)
C                                       Maximum number of random group
C                                       axes.
C
      INTEGER   NAXIS(MAXIS)
C                                       The number of pixels on each
C                                       axis.
C
      INTEGER   CHNIDX, IFIDX
C                                       The indices of the channel and
C                                       IF axes.  IFIDX = -1 if there
C                                       is no IF index.
C                                         1 <= CHNIDX <= MAXIS
C                                         1 <= IFIDX <= MAXIS or
C                                           IFIDX = -1
C
      DOUBLE PRECISION UVFREQ
C                                       The reference frequency for the
C                                       UV data.
C
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION FREQS(MAXCIF)
C                                       The frequency for each channel
C                                       and IF in the data in the order
C                                       in which they occur in the file.
C
      INTEGER   MAXPOL
      PARAMETER (MAXPOL = 4)
C                                       The maximum number of
C                                       polarizations expected in a file
C                                       .
C
      INTEGER   STKIDX
C                                       The index of the STOKES axis.
C
      CHARACTER POLLAB(MAXPOL)*2
C                                       Polarization labels (will either
C                                       be 'RR', 'LL', 'RL' and 'LR' or '
C                                       XX', 'YY', 'XY' and 'YX').
C
      INTEGER   POLCOD(MAXPOL)
C                                       Integer polarization codes
C                                       corresponding to POLLAB.
C
      LOGICAL   POLREQ(MAXPOL)
C                                       POLREQ(i) is true if the user
C                                       has requested a fringe search
C                                       for the polarization with label
C                                       POLLAB(i).  Not really a uv data
C                                       characteristic but still closely
C                                       associated with items from this
C                                       local include.
C
      INTEGER   POLNUM(MAXPOL)
C                                       The mapping from pixel numbers
C                                       on the STOKES axis to items in
C                                       POLLAB and POLREQ.
C
      INTEGER   TIMIDX, BASIDX, IDXA1, IDXA2
C                                       Indices of the time and baseline
C                                       parameters in the random
C                                       parameter array.
C
      INTEGER   IDSOUR
C                                       Current source number (from
C                                       index table)
C
      COMMON /UVINFO/ UVFREQ, FREQS, NAXIS, CHNIDX, IFIDX, STKIDX,
     *   POLCOD, POLREQ, POLNUM, TIMIDX, BASIDX, IDXA1, IDXA2, IDSOUR
      COMMON /UVINFC/ POLLAB
      SAVE /UVINFO/, /UVINFC/
C
LOCAL END
      PROGRAM BLING
C-----------------------------------------------------------------------
C! find residual rate and delays on individual baselines
C# CALIBRATION VLBI AP OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2006, 2010, 2012, 2015
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
C   This task finds fringes on individual baselines and stores them in
C   a Baseline Solution (BS) table.
C-----------------------------------------------------------------------
      CHARACTER INPUTS*6
      PARAMETER (INPUTS = 'Inputs')
C                                       The name of the inputs object.
      CHARACTER UVDATA*6
      PARAMETER (UVDATA = 'Uvdata')
C                                       The name of the UVDATA object
C                                       used for the input file.
      CHARACTER UVSCR*9
      PARAMETER (UVSCR = 'Uvscratch')
C                                       The name of the UV scratch file
C                                       object.
      INTEGER   IRET
C                                       Subroutine return status
C                                       indicator.
      INTEGER   DIEBUF(256)
C                                       Scratch buffer for DIE
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C
C     Prepare for the fringe search.  Read the values of the adverbs and
C     restart AIPS, copy the selected data to the scratch file and
C     divide by a model if the user requested this and it is appropriate
C     to do so.
C
      CALL SETUP (INPUTS, UVDATA, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
C
C     Process each baseline in turn.
C
      CALL PASS (UVDATA, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
C
C     Update history
C
      CALL BLHIST (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C
  990 CONTINUE
C
C     Terminate the program and return the final status code to the
C     AIPS monitor program.
C
      CALL DIE (IRET, DIEBUF)
      END
      BLOCK DATA INPDAT
C-----------------------------------------------------------------------
C   Initialize the adverb descriptions (see the comments for
C   'INPUTS.INC') above.
C-----------------------------------------------------------------------
      INTEGER   I, J
C                                       Loop indices for implied DO
C                                       loops.
      INCLUDE 'INPUTS.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
C-----------------------------------------------------------------------
      DATA (AVNAME(I), I = 1, 4) /'INNAME  ',
     *                            'INCLASS ',
     *                            'INSEQ   ',
     *                            'INDISK  '/
      DATA (AVTYPE(I), I = 1, 4) /OOACAR, OOACAR, OOAINT, OOAINT/
      DATA ((AVDIM(I, J), I = 1, 2), J = 1, 4) /12, 1,
     *                                           6, 1,
     *                                           1, 1,
     *                                           1, 1/
C                                       Input uv file.
C
      DATA (AVNAME(I), I = 5, 7) /'CALSOUR ',
     *                            'QUAL    ',
     *                            'CALCODE '/
      DATA (AVTYPE(I), I = 5, 7) /OOACAR, OOAINT, OOACAR/
      DATA ((AVDIM(I, J), I = 1, 2), J = 5, 7) /16, 30,
     *                                           1,  1,
     *                                           4,  1/
C                                       Source selection.
C
      DATA (AVNAME(I), I = 8, 20) /'STOKES  ',
     *                             'TIMERANG',
     *                             'ANTENNAS',
     *                             'BASELINE',
     *                             'SUBARRAY',
     *                             'SELBAND ',
     *                             'SELFREQ ',
     *                             'FREQID  ',
     *                             'BIF     ',
     *                             'EIF     ',
     *                             'BCHAN   ',
     *                             'ECHAN   ',
     *                             'UVRANGE '/
      DATA (AVTYPE(I), I = 8, 20) /OOACAR,
     *                             OOARE,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOARE,
     *                             OOARE,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOAINT,
     *                             OOARE/
      DATA ((AVDIM(I, J), I = 1, 2), J = 8, 20) / 4, 1,
     *                                            8, 1,
     *                                           50, 1,
     *                                           50, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            2, 1/
C                                       Data selection.
C
      DATA (AVNAME(I), I = 21, 29) /'DOCALIB ',
     *                              'GAINUSE ',
     *                              'DOPOL   ',
     *                              'PDVER   ',
     *                              'BLVER   ',
     *                              'FLAGVER ',
     *                              'DOBAND  ',
     *                              'BPVER   ',
     *                              'SMOOTH  '/
      DATA (AVTYPE(I), I = 21, 29) /OOARE,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOARE/
      DATA ((AVDIM(I, J), I = 1, 2), J = 21, 29) /1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            1, 1,
     *                                            3, 1/
C                                       Calibration.
C
      DATA (AVNAME(I), I = 30, 39) /'IN2NAME ',
     *                              'IN2CLASS',
     *                              'IN2SEQ  ',
     *                              'IN2DISK ',
     *                              'INVERS  ',
     *                              'NCOMP   ',
     *                              'FLUX    ',
     *                              'NMAPS   ',
     *                              'CMETHOD ',
     *                              'CMODEL  '/
      DATA (AVTYPE(I), I = 30, 39) /OOACAR,
     *                              OOACAR,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOAINT,
     *                              OOARE,
     *                              OOAINT,
     *                              OOACAR,
     *                              OOACAR/
      DATA ((AVDIM(I, J), I = 1, 2), J = 30, 39) /12, 1,
     *                                             6, 1,
     *                                             1, 1,
     *                                             1, 1,
     *                                             1, 1,
     *                                        MAXAFL, 1,
     *                                             1, 1,
     *                                             1, 1,
     *                                             4, 1,
     *                                             4, 1/
C                                       Model division.
C
      DATA (AVNAME(I), I = 40, 44) /'OPCODE  ',
     *                              'SOLINT  ',
     *                              'INFILE  ',
     *                              'APARM   ',
     *                              'DPARM   '/
      DATA (AVTYPE(I), I = 40, 44) /OOACAR,
     *                              OOARE,
     *                              OOACAR,
     *                              OOARE,
     *                              OOARE/
      DATA ((AVDIM(I, J), I = 1, 2), J = 40, 44) / 4, 1,
     *                                             1, 1,
     *                                            48, 1,
     *                                            10, 0,
     *                                            10, 0/
C                                       Task control.
C
      DATA (AVNAME(I), I = 45, 46) /'DOUVCOMP',
     *                              'BADDISK '/
      DATA (AVTYPE(I), I = 45, 46) /OOALOG,
     *                              OOAINT/
      DATA ((AVDIM(I, J), I = 1, 2), J = 45, 46) / 1, 1,
     *                                            10, 1/
C
      END
      SUBROUTINE SETUP (INPUTS, UVDATA, UVSCR, IRET)
C-----------------------------------------------------------------------
C   Initialize the AIPS run-time system, read and check the input adverb
C   values, copy the selected data to a scratch file (applying requested
C   calibrations) and divide by a model if required.
C
C   Inputs:
C      INPUTS      C*(*)           Name of inputs object used for
C                                  adverbs
C      UVDATA      C*(*)           Name of uvdata object used for the
C                                  input data data file
C      UVSCR       C*(*)           The uvdata object used for the
C                                  scratch file.
C
C   Outputs:
C      IRET        I               The return status:
C                                     0 => OK
C                                     anything else => failed
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), UVSCR*(*)
      INTEGER   IRET
C
      CHARACTER TNAME*6
      PARAMETER (TNAME = 'BLING ')
C                                       The task name.
      INCLUDE 'INPUTS.INC'
C-----------------------------------------------------------------------
C
C     Initialize AIPS and detach from the command-line processor.
C
      CALL AV2INP (TNAME, NPARMS, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Read control parameters.
C
      CALL RDCTRL (INPUTS, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Copy selected and calibrated data to a scratch file and divide
C     by a model if this has been requested and is appropriate.
C
      CALL CPDATA (INPUTS, UVDATA, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Read search parameters.
C
      CALL INISRH (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Obtain information about the UV data.
C
      CALL INIUVI (INPUTS, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Make a list of the selected baselines.
C
      CALL SETBSL (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
      END
      SUBROUTINE RDCTRL (INPUTS, IRET)
C-----------------------------------------------------------------------
C   Interpret the task control adverbs and store their values.
C
C   Inputs:
C      INPUTS   C*(*)      The name of the inputs object
C
C   Outputs:
C      IRET     I          The return status
C                              0 => OK
C                              anything else => failure
C
C   Outputs in COMMON:
C      DIVMOD   L          Divide model into data?
C      STACK    L          Stack baseline data?
C      CTHRSH   R          Coherence threshold (0.0 < CTHRSH <= 1.0)
C      MINSNR   R          SNR threshold (0.0 < MINSNR)
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   IRET
C
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls.
      INTEGER   DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls.
      REAL      APARM(10)
C                                       APARM input array
C
      CHARACTER OPCODE*4
C                                       Value of OPCODE adverb
C
      INCLUDE 'CONTROL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       BADDISK
      CALL OGET (INPUTS, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       parameters
      CALL INGET (INPUTS, 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Set minimum integration time in days
C
      MININT = APARM(1) / (24.0 * 60.0 * 60.0)
      IF (MININT.LE.0.0) THEN
         MININT = 1.0 / (24.0 * 60.0 * 60.0)
         END IF
C
C     Enable model division if APARM(2) is positive.
C
      DIVMOD = APARM(2).GT.0.0
C
C     Enable baseline stacking if APARM(3) is positive.
C
      STACK = APARM(3).GT.0.0
C
C     Read the minimum SNR from APARM(3) if APARM(3) is positive,
C     otherwise set the minimum SNR to 5.0.
C
      IF (APARM(4).LE.0.0) THEN
         MINSNR = 5.0
      ELSE
         MINSNR = APARM(3)
         END IF
C
C     Convert the percentage coherence threshold level in APARM(5) to
C     a fraction if APARM(5) is positive, otherwise set the coherence
C     threshold to 0.2.  If APARM(5) > 100 then set the coherence
C     threshold to unity.
C
      IF (APARM(5).GT.100.00) THEN
         CTHRSH = 1.0
         MSGTXT = 'Reducing coherence threshold to 100.0%'
         CALL MSGWRT (5)
      ELSE IF (APARM(5).GT.0.0) THEN
         CTHRSH = APARM(5) / 100.0
      ELSE
         CTHRSH = 0.2
         END IF
C
C     Get the precision control factors.
C
      DBLOAT = 1
      IF ((NINT (APARM(6)).GE.-6) .AND. (NINT(APARM(6)).LE.-1)) THEN
         DBLOAT = DBLOAT - NINT (APARM(6))
         END IF
      RBLOAT = 1
      IF ((NINT (APARM(7)).GE.-6) .AND. (NINT(APARM(7)).LE.-1)) THEN
         RBLOAT = RBLOAT - NINT (APARM(7))
         END IF
C
C     Get the solution mode.
C
      CALL INGET (INPUTS, 'OPCODE', TYPE, DIM, DUMMY, OPCODE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((OPCODE.EQ.'    ') .OR. (OPCODE.EQ.'INDE')) THEN
         MODE = 1
      ELSE IF (OPCODE.EQ.'VLBA') THEN
         MODE = 2
      ELSE IF (OPCODE.EQ.'MK3 ') THEN
         MODE = 3
      ELSE IF (OPCODE.EQ.'RATE') THEN
         MODE = 4
      ELSE
         MSGTXT = 'UNRECOGNIZED OPCODE: ''' // OPCODE // ''''
         CALL MSGWRT (10)
         GO TO 999
         END IF
C
  999 RETURN
      END
      SUBROUTINE CPDATA (INPUTS, UVDATA, UVSCR, IRET)
C-----------------------------------------------------------------------
C   Set up the data selection and calibration criteria for the input
C   file, copy calibrated and edited data to a scratch file and, if
C   required and appropriate, divide a model into the data in-place.
C   If IRET is 0 on return then the required data is in UVSCR.
C
C   Inputs:
C      INPUTS      C*(*)           Name of inputs object used for
C                                  adverbs
C      UVDATA      C*(*)           Name of uvdata object used for the
C                                  input data data file
C      UVSCR       C*(*)           The uvdata object used for the
C                                  scratch file.
C
C   Outputs:
C      IRET        I               The return status:
C                                     0 => OK
C                                     anything else => failed
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), UVSCR*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXMOD
      PARAMETER (MAXMOD = MAXFLD)
C                                       The maximum number of input
C                                       model files.
      CHARACTER MAP(MAXMOD)*6
C                                       The names of the objects used
C                                       for the models to be divided
C                                       into the data.
      INTEGER   NADVRB
      PARAMETER (NADVRB = 25)
C                                       The number of adverb values to
C                                       copy to the uv data set.
C
      CHARACTER ADVRB(NADVRB)*8, KEYWRD(NADVRB)*16
C                                       The adverbs to transfer and
C                                       their destination attribute
C                                       names.
C
      INTEGER   NADVB2
      PARAMETER (NADVB2 = 4)
C                                       The number of image name adverbs
C                                       to be transfered to the image
C                                       objects that are used to hold
C                                       the model.
      CHARACTER ADVRB2(NADVB2)*8, KEYWD2(NADVB2)*16
C                                       The adverbs to transfer and
C                                       their destination attribute
C                                       names.
C
      INTEGER   NADVB3
      PARAMETER (NADVB3 = 3)
C                                       The number of keywords
C                                       controlling model division
C
      CHARACTER ADVRB3(NADVB3)*8, KEYWD3(NADVB3)*16
C                                       The adverbs controlling model
C                                       division and their
C                                       destination keywords.
C
      INTEGER   SRCIDX
C                                       The random parameter number of
C                                       the SOURCE random parameter if
C                                       present or -1 if there is no
C                                       SOURCE random parameter.
      INTEGER   NMAPS, NCLNG(MAXFLD)
      LOGICAL   NONEGS, SOMSET
C                                       The number of clean map files.
C                                       (NMAPS <= MAXFLD)
C                                       Number Clean comps
C                                       Include negative CCs?
      INTEGER   IMAP
C                                       Map counter
C
      LOGICAL   CMPSCR
C                                       Compress scratch file?
C
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
      INTEGER   DUMMY
C                                       Dummy numeric argument used
C                                       in AIPS object system calls.
C
      CHARACTER IN2NAM
C                                       The value of the IN2NAME
C                                       adverb.
C
      CHARACTER CLASS*6
C                                       The class name of the current
C                                       image.
C
      CHARACTER NUM*2
C                                       The numberic suffix to be added
C                                       to the classname for the current
C                                       image.
C
      INTEGER   INVERS
C                                       The CC table version number.
C
      INTEGER   CCVERS(MAXFLD)
C                                       The CC table version number
C                                       for each field.
C
      INTEGER   MAXIS
      PARAMETER (MAXIS = 7)
C                                       The maximum number of axes in a
C                                       data group.
C
      INTEGER   NAXIS(MAXIS)
C                                       The shape of a data group.
C
      INTEGER   NCHAN
C                                       The combined number of channels
C                                       and IFs.
C
      INTEGER   FRQIDX, IFIDX, J0, JJ
C                                       The indices of the frequency and
C                                       IF axes (if present)
      REAL      XDOCAL
      LOGICAL   LTEMP, OLDNAM
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'CONTROL.INC'
C
      DATA ADVRB /'INNAME  ', 'INCLASS', 'INSEQ   ', 'INDISK  ',
     *   'CALSOUR ', 'QUAL    ', 'CALCODE ', 'TIMERANG', 'SUBARRAY',
     *   'SELBAND ', 'SELFREQ ', 'FREQID  ', 'BIF     ', 'EIF     ',
     *   'BCHAN   ', 'ECHAN   ', 'UVRANGE ', 'GAINUSE ', 'DOPOL   ',
     *   'BLVER   ', 'FLAGVER ', 'DOBAND  ', 'BPVER   ', 'SMOOTH  ',
     *   'PDVER   '/
      DATA KEYWRD /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *   'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  ', 'CALEDIT.SOURCS  ',
     *   'CALEDIT.SELQUA  ', 'CALEDIT.SELCOD  ', 'CALEDIT.TIMRNG  ',
     *   'CALEDIT.SUBARR  ', 'CALEDIT.SELBAN  ', 'CALEDIT.SELFRQ  ',
     *   'CALEDIT.FRQSEL  ', 'CALEDIT.BIF     ', 'CALEDIT.EIF     ',
     *   'CALEDIT.BCHAN   ', 'CALEDIT.ECHAN   ', 'CALEDIT.UVRNG   ',
     *   'CALEDIT.CLUSE   ', 'CALEDIT.DOPOL   ', 'CALEDIT.BLVER   ',
     *   'CALEDIT.FGVER   ', 'CALEDIT.DOBAND  ', 'CALEDIT.BPVER   ',
     *   'CALEDIT.SMOOTH  ', 'CALEDIT.PDVER   '/
      DATA ADVRB2 /'IN2NAME ', 'IN2CLASS', 'IN2SEQ  ', 'IN2DISK '/
      DATA KEYWD2 /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *   'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
      DATA ADVRB3 /'FLUX    ', 'CMETHOD ', 'CMODEL  '/
      DATA KEYWD3 /'MODFLUX        ', 'MODMETH         ',
     *   'MODMODEL        '/
C-----------------------------------------------------------------------
C
C     Create the input uvdata object and copy the adverbs specifying the
C     input data file and the data selection and calibration to be
C     applied to the data to the object.
C
      CALL OUVCRE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ (INPUTS, NADVRB, ADVRB, KEYWRD, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Inputs', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVDATA, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Turn off model division unless the input file is a single-
C     source file or only one source has been selected using the source
C     selection parameters.  In both of these cases UVDFND will return
C     -1 for SRCIDX since the object system will mask the SOURCE
C     parameter for a multisource file if only one source is selected
C     using the source selection parameters.  IRET will be 1 if the
C     SOURCE axis is not present: this should be ignored.
C
      CALL UVDFND (UVDATA, 1, 'SOURCE', SRCIDX, IRET)
      IF (IRET.EQ.0) THEN
C                                       If IRET is zero then a SOURCE
C                                       random parameter was found.
         IF (DIVMOD) THEN
            MSGTXT =
     *         'I will not divide a model into the data because '//
     *         'more than'
            CALL MSGWRT (5)
            MSGTXT = 'one source has been selected.'
            CALL MSGWRT (5)
            END IF
         DIVMOD = .FALSE.
C
C        Clear the error status.
C
         IRET = 0
      ELSE IF (IRET.NE.1) THEN
C                                       IRET is 1 if the source axis was
C                                       not found but UVDFND was
C                                       otherwise successful.
         GO TO 999
         END IF
C
C     Copy the selected data to the scratch file.
C
      MSGTXT = 'Copying the selected data to a scratch file.'
      CALL MSGWRT (4)
      CALL INGET (INPUTS, 'DOUVCOMP', TYPE, DIM, CMPSCR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UV2SCR (UVDATA, UVSCR, CMPSCR, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Find the number of clean map files; turn off model division if
C     this is zero.
C
      CALL INGET (INPUTS, 'NMAPS', TYPE, DIM, NMAPS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NMAPS.LE.0) DIVMOD = .FALSE.
C
C     Turn off model division if IN2NAME was left blank.
C
      CALL INGET (INPUTS, 'IN2NAME', TYPE, DIM, DUMMY, IN2NAM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (IN2NAM.EQ.' ') DIVMOD = .FALSE.
C
C     Divide the model into the data, if required, overwriting the
C     scratch file.
C
      IF (DIVMOD) THEN
C
C        Fill in the file names for the map files containg the model.
C        The first is taken from the IN2NAME inputs while the others
C        have identical names apart from a number appended to the
C        class name.  Check that each image file exists by opening it
C        read-only.
C
         MAP(1) = 'Map-01'
         CALL IMGCRE (MAP(1), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL IN2OBJ (INPUTS, NADVB2, ADVRB2, KEYWD2, MAP(1), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL IMGOPN (MAP(1), 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL FNAGET (MAP(1), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         OLDNAM = (CLASS(4:4).LT.'0') .OR. (CLASS(4:4).GT.'9') .OR.
     *      (CLASS(5:5).LT.'0') .OR. (CLASS(5:5).GT.'9') .OR.
     *      (CLASS(6:6).LT.'0') .OR. (CLASS(6:6).GT.'9')
         IF (.NOT.OLDNAM) THEN
            IF ((CLASS(3:3).LT.'0') .OR. (CLASS(3:3).GT.'9')) THEN
               READ (CLASS(4:6),'(I3)') J0
            ELSE
               READ (CLASS(3:6),'(I4)') J0
               END IF
            END IF
         DO 10 IMAP = 2,NMAPS
            WRITE (NUM, '(I2.2)') IMAP
            MAP(IMAP) = 'Map-' // NUM
            CALL IMGCRE (MAP(IMAP), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL IN2OBJ (INPUTS, NADVB2, ADVRB2, KEYWD2, MAP(IMAP),
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL FNAGET (MAP(IMAP), 'CLASS', TYPE, DIM, DUMMY, CLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IF (OLDNAM) THEN
               JJ = IMAP - 1
               CALL ZEHEX (JJ, 2, CLASS(5:6))
            ELSE
               JJ = IMAP - 1 + J0
               IF (JJ.LE.999) THEN
                  WRITE (CLASS(4:6),'(I3.3)') JJ
               ELSE
                  WRITE (CLASS(3:6),'(I4.4)') JJ
                  END IF
               END IF
            CALL FNAPUT (MAP(IMAP), 'CLASS', TYPE, DIM, DUMMY, CLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL IMGOPN (MAP(IMAP), 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
 10         CONTINUE
C
C        Add inputs to control the division to the input file.
C
         CALL IN2OBJ (INPUTS, NADVB3, ADVRB3, KEYWD3, UVSCR, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        The CC table version number has to be copied for each field.
C
         CALL INGET (INPUTS, 'INVERS', TYPE, DIM, INVERS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 20 IMAP = 1,NMAPS
            CCVERS(IMAP) = INVERS
   20       CONTINUE
         TYPE = OOAINT
         DIM(1) = NMAPS
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVSCR, 'MODCCVER', TYPE, DIM, CCVERS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       handle NCOMP and noneg
         CALL FILL (MAXFLD, 0, NCLNG)
         CALL INGET (INPUTS, 'NCOMP', TYPE, DIM, NCLNG, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         NONEGS = .FALSE.
         SOMSET = .FALSE.
         DO 25 IMAP = 1,NMAPS
            IF (NCLNG(IMAP).LT.0) THEN
               NONEGS = .TRUE.
               NCLNG(IMAP) = -NCLNG(IMAP)
               END IF
            IF (NCLNG(IMAP).GT.0) SOMSET = .TRUE.
 25         CONTINUE
         IF ((SOMSET) .AND. (NMAPS.GT.MAXAFL)) CALL FILL (MAXFLD-MAXAFL,
     *      1000000000, NCLNG(MAXAFL+1))
         DIM(1) = MAXFLD
         CALL OPUT (UVSCR, 'MODCCEND', TYPE, DIM, NCLNG, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         TYPE = OOALOG
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVSCR, 'MODNONEG', TYPE, DIM, NONEGS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Enable progress reporting.
C
         CALL OPUT (UVSCR, 'MODDOMSG', TYPE, DIM, .TRUE., CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        Find the number of channels and IFs in the data.
C
         CALL UVDGET (UVSCR, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (DIM(1).GT.MAXIS) THEN
            WRITE (MSGTXT,
     *         '(''OVERFLOWED AXIS ARRAY ('', I2, '' > '', I2, '')'')')
     *         DIM(1), MAXIS
            CALL MSGWRT (10)
            MSGTXT = 'REPORT THIS ERROR TO THE AIPS GROUP'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         CALL UVDFND (UVSCR, 2, 'FREQ', FRQIDX, IRET)
         IF (IRET.EQ.1) THEN
            MSGTXT = 'BAD DATA SET (NO FREQ AXIS)'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         NCHAN = NAXIS(FRQIDX)
         CALL UVDFND (UVSCR, 2, 'IF', IFIDX, IRET)
         IF (IRET.EQ.0) THEN
            NCHAN = NCHAN * NAXIS(IFIDX)
         ELSE IF (IRET.NE.1) THEN
            GO TO 999
            END IF
C
C        Now we have the information necessary to perform the
C        division.
C
         MSGTXT = 'Dividing the data by the model'
         CALL MSGWRT (4)
         CALL UVDIVM (UVSCR, UVSCR, NMAPS, MAP, 1, NCHAN, IRET)
         IF (IRET.NE.0) GO TO 999
C
C        The image files are finished with.  Clean up the associated
C        objects.
C
         DO 30 IMAP = 1, NMAPS
            CALL IMGCLO (MAP(IMAP), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL IMGDES (MAP(IMAP), IRET)
            IF (IRET.NE.0) GO TO 999
   30       CONTINUE
         END IF
C
  999 RETURN
      END
      SUBROUTINE INIUVI (INPUTS, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Obtain information about UVFILE and store it in the COMMON blocks
C   specified in UVINFO.INC.
C
C   Input
C      INPUTS    C*(*)     The name of an INPUTS object containing
C                          adverb values
C      UVFILE    C*(*)     The name of a UVDATA object
C
C   Output
C      IRET      I         Return code: 0 => normal return
C                                       anything else => error
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVFILE*(*)
      INTEGER   IRET
C
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
C
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
C
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INCLUDE 'UVINFO.INC'
C
      DOUBLE PRECISION CRVAL(MAXIS)
C                                       Coordinates of the reference
C                                       pixel for each axis.
C
      REAL      CDELT(MAXIS)
C                                       Coordinate increment for
C                                       each axis.
C
      REAL      CRPIX(MAXIS)
C                                       Reference pixel for each axis
C
      CHARACTER LPLAB(MAXPOL)*2, CPLAB(MAXPOL)*2
C                                       Polarizations labels for linear
C                                       and circular polarization.
C
      INTEGER   LPCODE(MAXPOL), CPCODE(MAXPOL)
C                                       Polarization code for linear and
C                                       circular polarization.
C
      LOGICAL   CPOL
C                                       Does the file contain circular
C                                       polarization data?
C
      INTEGER   POL
C                                       Polarization index.
C
      INTEGER   PN
C                                       AIPS polarization number
C
      CHARACTER STOKES*4
C                                       Polarization request
C
      INTEGER   NPREQ
C                                       Number of polarizations
C                                       requested.
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA LPLAB /'XX', 'YY', 'XY', 'YX'/
      DATA CPLAB /'RR', 'LL', 'RL', 'LR'/
      DATA LPCODE /-5, -6, -7, -8/
      DATA CPCODE /-1, -2, -3, -4/
C-----------------------------------------------------------------------
C
C     Get the shape of the group array and the indices of the channel
C     IF axes:
C
      CALL UVDGET (UVFILE, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVFILE, 2, 'FREQ    ', CHNIDX, IRET)
      IF (IRET.EQ.1) THEN
         MSGTXT = 'INPUT FILE IS BAD (NO FREQ AXIS)'
         CALL MSGWRT (10)
         END IF
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVFILE, 2, 'IF      ', IFIDX, IRET)
      IF (IRET.EQ.1) IRET = 0
C                                       Not an error if no IF axis
      IF (IRET.NE.0) GO TO 999
C
C     Get frequency information
C
      CALL UVFRQS (UVFILE, UVFREQ, FREQS, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Find the STOKES axis
C
      CALL UVDFND (UVFILE, 2, 'STOKES  ', STKIDX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'INPUT FILE IS BAD (NO STOKES AXIS)'
         CALL MSGWRT (10)
         END IF
C
C     Get the reference pixel values and the coordinate increments
C
      CALL UVDGET (UVFILE, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVFILE, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVFILE, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Does this look like circularly polarized data?
C
      CPOL = (NINT (CRVAL(STKIDX)).LE.-1) .AND.
     *   (NINT (CRVAL(STKIDX)).GE.-4)
      DO 10 POL = 1, MAXPOL
         IF (CPOL) THEN
            POLLAB(POL) = CPLAB(POL)
            POLCOD(POL) = CPCODE(POL)
         ELSE
            POLLAB(POL) = LPLAB(POL)
            POLCOD(POL) = LPCODE(POL)
            END IF
 10      CONTINUE
C
C     Figure out polarization numbers
C
      DO 20 POL = 1, NAXIS(STKIDX)
         PN = NINT (CRVAL(STKIDX) + CDELT(STKIDX)
     *      * (POL - CRPIX(STKIDX)))
         IF (PN.GE.0) THEN
            MSGTXT = 'ERROR - INPUT FILE CONTAINS TRUE STOKES DATA'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         IF (CPOL) THEN
            IF ((PN.GE.-4) .AND. (PN.LE.-1)) THEN
               POLNUM(POL) = -PN
            ELSE
               MSGTXT = 'ERROR - INPUT FILE MIXES POLARIZATION TYPES'
               CALL MSGWRT (10)
               GO TO 999
               END IF
         ELSE
            IF ((PN.GE.-8) .AND. (PN.LE.-5)) THEN
               POLNUM(POL) = -PN - 4
            ELSE
               MSGTXT = 'ERROR - INPUT FILE MIXES POLARIZATION TYPES'
               CALL MSGWRT (10)
               GO TO 999
               END IF
            END IF
 20      CONTINUE
C
C     Sort out polarization requests. Individual requests are checked
C     first followed by special strings ('FULL' and 'CROS').  If the
C     STOKES value is not a recognized string then it is assumed that
C     the user wants both parallel hands.
C
      CALL INGET (INPUTS, 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 30 POL = 1,MAXPOL
         POLREQ(POL) = .FALSE.
 30      CONTINUE
      IF ((STOKES(1:2).EQ.POLLAB(1)) .OR.
     *   (STOKES(3:4).EQ.POLLAB(1))) THEN
         POLREQ(1) = .TRUE.
      ELSE IF ((STOKES(1:2).EQ.POLLAB(2)) .OR.
     *      (STOKES(3:4).EQ.POLLAB(2))) THEN
         POLREQ(2) = .TRUE.
      ELSE IF ((STOKES(1:2).EQ.POLLAB(3)) .OR.
     *      (STOKES(3:4).EQ.POLLAB(3))) THEN
         POLREQ(3) = .TRUE.
      ELSE IF ((STOKES(1:2).EQ.POLLAB(4)) .OR.
     *      (STOKES(3:4).EQ.POLLAB(4))) THEN
         POLREQ(4) = .TRUE.
      ELSE IF (STOKES.EQ.'FULL') THEN
         DO 40 POL = 1, MAXPOL
            POLREQ(POL) = .TRUE.
 40         CONTINUE
      ELSE IF (STOKES.EQ.'CROS') THEN
         DO 50 POL = 3,MAXPOL
            POLREQ(POL) = .TRUE.
 50         CONTINUE
      ELSE
         DO 60 POL = 1,2
            POLREQ(POL) = .TRUE.
 60         CONTINUE
         END IF
C
C     Check the number of requested polarizations
C
      NPREQ = 0
      DO 70 POL = 1, MAXPOL
         IF (POLREQ(POL)) THEN
            NPREQ = NPREQ + 1
            END IF
   70    CONTINUE
      IF (NPREQ.EQ.0) THEN
         MSGTXT = 'NO POLARIZATIONS REQUESTED - CHECK STOKES ADVERB'
         CALL MSGWRT (10)
         GO TO 999
         END IF
C
C     Get random parameter indices.
C
      CALL UVDFND (UVFILE, 1, 'TIME1   ', TIMIDX, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'INPUT FILE IS BAD - NO TIME1 PARAMETER'
         CALL MSGWRT (10)
         GO TO 999
         END IF
      CALL UVDFND (UVFILE, 1, 'BASELINE', BASIDX, IRET)
      IF (IRET.NE.0) THEN
         CALL UVDFND (UVFILE, 1, 'ANTENNA1', IDXA1, IRET)
         IF (IRET.EQ.0) CALL UVDFND (UVFILE, 1, 'ANTENNA2', IDXA2, IRET)
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'INPUT FILE IS BAD - NO BASELINE PARAMETER'
         CALL MSGWRT (10)
         GO TO 999
         END IF
C
  999 RETURN
      END
      SUBROUTINE INISRH (INPUTS, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Initialize the list of search parameters from the input adverbs and
C   from the control file.
C
C   Inputs:
C      INPUTS    C*(*)     Name of INPUTS object
C      UVFILE    C*(*)     Name of input UVDATA object
C
C   Output
C      IRET      I         Status code: 0 => normal completion
C                                       anything else => error
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVFILE*(*)
      INTEGER   IRET
C
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
C
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
C
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
      INTEGER   DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls).
      REAL      SOLINT
C                                       Solution interval in minutes
C
      REAL      DPARM(10)
C                                       DPARM adverb values array.
C
      CHARACTER FNAME*48
C                                       Input file name.
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SEARCH.INC'
C-----------------------------------------------------------------------
C
C     Fill the first entry using adverb values.  This is the default
C     setting.
C
      NMSRCH = 1
      SRCHBL(1, 1) = 0
      SRCHBL(2, 1) = 0
      CALL INGET (INPUTS, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      SRCHSI(1) = SOLINT / (24.0 * 60.0)
      SRCHTI(1, 1) = 0.0
      SRCHTI(2, 1) = 9999.0
      CALL INGET (INPUTS, 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      SRCHWN(1, 1, 1) = DPARM(1) / 1.0E9
      SRCHWN(2, 1, 1) = DPARM(2) / 1.0E9
      SRCHWN(1, 2, 1) = DPARM(3) / 1.0E9
      SRCHWN(2, 2, 1) = DPARM(4) / 1.0E9
      SRCHWN(1, 3, 1) = DPARM(5) / 1.0E3
      SRCHWN(2, 3, 1) = DPARM(6) / 1.0E3
      SRCHWN(1, 4, 1) = DPARM(7) / 1.0E6
      SRCHWN(2, 4, 1) = DPARM(8) / 1.0E6
      SRCHDA(1) = DPARM(9) / 1.0E6
C
C     Check for a control file.
C
      CALL INGET (INPUTS, 'INFILE', TYPE, DIM, DUMMY, FNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (FNAME.NE.'  ') THEN
         CALL RDCFIL (FNAME, UVFILE, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
  999 RETURN
      END
      SUBROUTINE RDCFIL (FNAME, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Read the named control file.
C
C   Inputs:
C      FNAME   C*(*)   The name of the control file.
C      UVFILE  C*(*)   The name of the UVDATA object.
C
C   Outputs:
C      IRET    I       Status: 0 => normal completion
C                              anything else => error
C-----------------------------------------------------------------------
      CHARACTER FNAME*(*), UVFILE*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   TXTLUN, TXTIND
      PARAMETER (TXTLUN = 10)
C                                       AIPS LUN and FTAB index for the
C                                       control file.
C
      INTEGER   NPARM
      PARAMETER (NPARM = MAXANT + 1)
C                                       The number of KEYIN keywords for
C                                       baseline-group headers.
C
      CHARACTER KEYS(NPARM)*8
C                                       The keywords that are recognized
C                                       in baseline-group headers.
C                                       These are the antenna names and
C                                       the word "ANY".
      INTEGER   NDUMMY
      PARAMETER (NDUMMY = 200 * 14)
C                                       The number of dummy keywords
C                                       used for reading baseline-group
C                                       data (up to 200 entries per
C                                       group).
C
      CHARACTER DUMKEY(NDUMMY)*8
C                                       The list of dummy keywords.
C
      DOUBLE PRECISION VALNUM(NDUMMY)
      CHARACTER VALCHR(NDUMMY)*8
C                                       Numeric and character values
C                                       returned by KEYIN.
C
      CHARACTER ENDMRK*8
      PARAMETER (ENDMRK = '/       ')
C                                       The character used to terminate
C                                       baseline-group data.
C
      INTEGER   SUBARR
C                                       The subarray number
C
      CHARACTER ANTTBL*12
      PARAMETER (ANTTBL = 'Antenna table')
C                                       The name of the TABLE object
C                                       used to access the antenna
C                                       table.
C
      CHARACTER OBSDAT*8
C                                       The observing date as a string.
C
      INTEGER   OBSDMY(3)
C                                       The observing date broken down
C                                       into a day, month and year
C                                       number.
C
      INTEGER   CAL(12)
C                                       The number of days elapsed
C                                       between Jan 1st and the first of
C                                       each month in a non-leap year.
C
      INTEGER   OBSDNO
C                                       The annual day number of the
C                                       observing date.
C
      INTEGER   A1, A2
C                                       First and second antenna number
C                                       for the current baseline group
C                                       (zero corresponds to 'ANY').
C
      INTEGER   NDATA
C                                       Number of data items read by
C                                       KEYIN.
C
      INTEGER   IDX
C                                       Index of the current record in a
C                                       data block.
C
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
C
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
C
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   I
C                                       Loop index
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SEARCH.INC'
C
      DATA KEYS /MAXANT * '        ', 'ANY     '/
      DATA DUMKEY /NDUMMY * '        '/
      DATA CAL /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
C-----------------------------------------------------------------------
C
C     Use the antenna names from the antenna table as keywords for the
C     baseline-group headers.
C
      CALL SECGET (UVFILE, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UV2TAB (UVFILE, ANTTBL, 'AN', SUBARR, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 10 I = 1, MAXANT
         CALL ANTNFO (ANTTBL, SUBARR, I, 'ANNAME  ', TYPE, DIM, DUMMY,
     *      KEYS(I), IRET)
         IF (IRET.NE.0) GO TO 999
   10    CONTINUE
C                                       Finished with AN table.
      CALL TABDES (ANTTBL, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Find the annual day number corresponding to the reference
C     date for the observations.
C
      CALL UVDGET (UVFILE, 'DATE-OBS', TYPE, DIM, DUMMY, OBSDAT, IRET)
      IF (IRET.NE.0) GO TO 999
      READ (OBSDAT, '(I2, ''/'', I2, ''/'', I2)') OBSDMY(1), OBSDMY(2),
     *   OBSDMY(3)
      OBSDNO = CAL(OBSDMY(2)) + OBSDMY(1)
      IF ((MOD(OBSDMY(3), 4).EQ.0).AND.(OBSDMY(2).GT.2)) THEN
         OBSDNO = OBSDNO + 1
         END IF
C                                       This simple leap-year correction
C                                       works until 2100.
C
C     Open the input file.
C
      CALL ZTXOPN ('READ', TXTLUN, TXTIND, FNAME, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Read the file.
C
  100 CONTINUE
C
C        Fill the values arrays with default values.
C
         DO 110 I = 1, NPARM
            VALNUM(I) = -1.0
            VALCHR(I) = '        '
  110       CONTINUE
         VALNUM(NPARM + 1) = 0.0
C
C        Read the next baseline-group header.
C
         CALL KEYIN (KEYS, VALNUM, VALCHR, NPARM, ENDMRK, 1,
     *      TXTLUN, TXTIND, IRET)
         IF (IRET.EQ.1) GO TO 150
C                                       Exits loop
         IF (IRET.NE.0) GO TO 999
C
C        Find the antenna numbers.
C
         A1 = 1
C                                       Simulated while loop
C                                       Invariant: VALNUM(1:A1-1) = -1.0
C                                       Variant: NPARM - A1 + 1
  120    IF ((A1.LE.NPARM) .AND. (VALNUM(A1).EQ.-1.0)) THEN
            A1 = A1 + 1
            GO TO 120
            END IF
         IF (A1.GT.NPARM) THEN
C                                       Header contains no recognized
C                                       keywords.
            MSGTXT = 'BAD HEADER - NO RECOGNIZED ANTENNA NAMES'
            CALL MSGWRT (10)
            IRET = 1
            GO TO 999
         ELSE IF (A1.EQ.NPARM) THEN
C                                       Baseline group applies to all
C                                       baselines.
            A1 = 0
            A2 = 0
         ELSE
C                                       Need to find a second antenna.
            A2 = A1 + 1
C                                       Simulated while loop
C                                       Invariant: VALNUM(1:A1-1) = -1.0
C                                       Variant: NPARM - A1 + 1
  130       IF ((A2.LE.NPARM) .AND. (VALNUM(A2).EQ.-1.0)) THEN
               A2 = A2 + 1
               GO TO 130
               END IF
            IF (A2.GT.NPARM) THEN
C                                       No second antenna.
               MSGTXT = 'BAD HEADER - NEED TWO RECOGNIZED ANTENNA NAMES'
               CALL MSGWRT (10)
               IRET = 2
               GO TO 999
            ELSE IF (A2.EQ.NPARM) THEN
               A2 = A1
               A1 = 0
               END IF
            END IF
C
C        Read the data block.
C
         NDATA = NDUMMY
         CALL KEYIN (DUMKEY, VALNUM, VALCHR, NDATA, ENDMRK, 4,
     *      TXTLUN, TXTIND, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR READING DATA BLOCK'
            CALL MSGWRT (10)
            GO TO 999
            END IF
         IF (MOD (NDATA, 14).NE.0) THEN
            MSGTXT = 'DATA BLOCK CONTAINS AN INCOMPLETE RECORD'
            CALL MSGWRT (10)
            GO TO 999
            END IF
C
         DO 140 I = 1, NDATA / 14
            IF (NMSRCH.EQ.MXSRCH) THEN
               WRITE (MSGTXT, '(A, I3, A)') 'TOO MANY RECORDS (> ',
     *            MXSRCH - 1, ') IN CONTROL FILE'
               CALL MSGWRT (10)
               GO TO 999
               END IF
            NMSRCH = NMSRCH + 1
            SRCHBL(1, NMSRCH) = A1
            SRCHBL(2, NMSRCH) = A2
            IDX = 14 * (I - 1)
            SRCHTI(1, NMSRCH) = (VALNUM(IDX + 1) - OBSDNO)
     *         + VALNUM (IDX + 2) / 24.0
            SRCHTI(2, NMSRCH) = (VALNUM(IDX + 3) - OBSDNO)
     *         + VALNUM (IDX + 4) / 24.0
            SRCHSI(NMSRCH) = VALNUM (IDX + 5) / (24.0 * 60.0)
            SRCHWN(1, 1, NMSRCH) = VALNUM(IDX + 6) / 1.0E9
            SRCHWN(2, 1, NMSRCH) = VALNUM(IDX + 7) / 1.0E9
            SRCHWN(1, 2, NMSRCH) = VALNUM(IDX + 8) / 1.0E9
            SRCHWN(2, 2, NMSRCH) = VALNUM(IDX + 9) / 1.0E9
            SRCHWN(1, 3, NMSRCH) = VALNUM(IDX + 10) / 1.0E3
            SRCHWN(2, 3, NMSRCH) = VALNUM(IDX + 11) / 1.0E3
            SRCHWN(1, 4, NMSRCH) = VALNUM(IDX + 12) / 1.0E6
            SRCHWN(2, 4, NMSRCH) = VALNUM(IDX + 13) / 1.0E6
            SRCHDA(NMSRCH) = VALNUM(IDX + 14) / 1.0E6
  140       CONTINUE
      GO TO 100
C
  150    CONTINUE
C
C     Close the control file.
C
      CALL ZTXCLS (TXTLUN, TXTIND, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
      END
      INTEGER FUNCTION SRCHPM (A1, A2, T)
C-----------------------------------------------------------------------
C   The index of the search parameters for baseline A1-A2 at time T.
C
C   Inputs:
C      A1     I      First antenna number
C                    0 < A1 < MAXANT
C      A2     I      Second antenna number
C                    A1 < A2 <= MAXANT
C      T      R      Time in days from 0h on the reference date
C-----------------------------------------------------------------------
      INTEGER   A1, A2
      REAL      T
C
      INTEGER   BEST
C                                       Index of best match yet found
C
      INTEGER   I
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SEARCH.INC'
C-----------------------------------------------------------------------
      BEST = 1
C                                       The first entry is guaranteed
C                                       to apply to all baselines at all
C                                       times.
      DO 10 I = 2, NMSRCH
         IF ((SRCHTI(1, I).LE.T) .AND. (SRCHTI(2, I).GE.T)) THEN
C                                       Time range is OK.
            IF ((SRCHBL(1, I).EQ.A1) .AND. (SRCHBL(2, I).EQ.A2))
     *         THEN
C                                       Exact match, overrides current
C                                       best if it covers a smaller time
C                                       range.
               IF ((SRCHTI(2, I) - SRCHTI(1, I)) .LT.
     *            (SRCHTI(2, BEST) - SRCHTI(1, BEST))) THEN
                  BEST = I
                  END IF
            ELSE IF ((SRCHBL(1, I).EQ.0) .AND. ((SRCHBL(2, I).EQ.A1)
     *            .OR. (SRCHBL(2, I).EQ.A2))) THEN
C                                       One wild-card: overrides two
C                                       wild-cards or one wildcard and a
C                                       looser timerange.
               IF (SRCHBL(2, BEST).EQ.0) THEN
C                                       Best had two wildcards since
C                                       SRCHBL(2, i) = 0 =>
C                                         SRCHBL(1,i) = 0
                  BEST = I
               ELSE IF ((SRCHBL(1, BEST).EQ.0) .AND.
     *               ((SRCHTI(2, I) - SRCHTI(1, I)) .LT.
     *               (SRCHTI(2, BEST) - SRCHTI(1, BEST)))) THEN
                  BEST = I
                  END IF
            ELSE IF (SRCHBL(2, I).EQ.0) THEN
C                                       Two wild-cards: only overrides
C                                       two wild-cards with looser
C                                       timerange.
               IF ((SRCHBL(2, BEST).EQ.0) .AND.
     *            ((SRCHTI(2, I) - SRCHTI(1, I)) .LT.
     *            (SRCHTI(2, BEST) - SRCHTI(1, BEST)))) THEN
                  BEST = I
                  END IF
               END IF
            END IF
   10    CONTINUE
C
      SRCHPM = BEST
      END
      SUBROUTINE SETBSL (ADVRBS, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Construct a list of requested baselines from the SUBARR, ANTENNAS
C   and BASELINE adverbs.  Common blocks in BASELN.INC are initialized.
C
C   Inputs:
C      ADVRBS    C*(*)      The name of the INPUTS object holding the
C                           input adverb values.
C      UVFILE    C*(*)      The name of the UVDATA object associated
C                           with the input file (which should be open).
C
C   Output:
C      IRET      I          Return status: 0 => list created
C                                          anything else => failure
C------------------------------------------------------------------------
      CHARACTER ADVRBS*(*), UVFILE*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER ANTTBL*16
      PARAMETER (ANTTBL = 'Antenna Table')
C                                       The name of the TABLE object
C                                       used to access the AN table.
C
      INTEGER   SUBARR
C                                       The requested subarray number.
C
      INTEGER   A(50), NA
C                                       The absolute values of the
C                                       ANTENNAS array with zeros
C                                       removed and the number of non
C                                       -zero items.
C                                       0 <= NA <= 50
C
      INTEGER   B(50), NB
C                                       The absolute values of the
C                                       BASELINE array with zeros
C                                       removed and the number of non
C                                       -zero items.
C                                       0 <= NB <= 50
C
      LOGICAL   REJECT
C                                       The reject flag: if true then
C                                       baselines specified with the
C                                       ANTENNAS/BASELINE adverbs are
C                                       rejected rather than selected.
C
C
      CHARACTER ANNAME*8
C                                       Antenna name buffer
      INTEGER   TYPE
C                                       Type code for AIPS object
C                                       attributes.
C
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
C
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   I, J
C                                       Loop indices.
C
      LOGICAL   REQBAS
      EXTERNAL  REQBAS
C
      INCLUDE 'BASELN.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Get the subarray number and handle default values:
C
      CALL INGET (ADVRBS, 'SUBARRAY', TYPE, DIM, SUBARR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SUBARR.LE.0) THEN
         MSGTXT = 'Using default value of 1 for SUBARRAY'
         CALL MSGWRT (4)
         SUBARR = 1
         CALL INPUTT (ADVRBS, 'SUBARRAY', TYPE, DIM, SUBARR, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL SECPUT (UVFILE, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
C     Find out which antenna IDs occurr in the subarray.  This is done
C     by looking up the name corresponding to each possible ID number
C     and seeing whether or not it is blank.
C
      CALL UV2TAB (UVFILE, ANTTBL, 'AN', SUBARR, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 10 I = 1, MAXANT
         ANNAME = '        '
         CALL ANTNFO (ANTTBL, SUBARR, I, 'ANNAME', TYPE, DIM, DUMMY,
     *      ANNAME, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (ANNAME.EQ.'        ') THEN
            AEXIST(I) = .FALSE.
         ELSE
            AEXIST(I) = .TRUE.
            END IF
   10    CONTINUE
C
C     Read the ANTENNA and BASELINE adverbs:
C
      CALL INGET (ADVRBS, 'ANTENNAS', TYPE, DIM, A, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL INGET (ADVRBS, 'BASELINE', TYPE, DIM, B, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Make all entries positive, remove zeros and record the
C     sense of the selection:
C
      NA = 0
      NB = 0
      REJECT = .FALSE.
      DO 20 I = 1, 50
         IF (A(I).NE.0) THEN
            IF (A(I).LT.0) REJECT = .TRUE.
            NA = NA + 1
            A(NA) = ABS (A(I))
            END IF
         IF (B(I).NE.0) THEN
            IF (B(I).LT.0) REJECT = .TRUE.
            NB = NB + 1
            B(NB) = ABS (B(I))
            END IF
 20      CONTINUE
C
C     List requested baselines:
C
      DO 40 I = 1, MAXANT - 1
         IF (AEXIST(I)) THEN
            DO 30 J = I + 1, MAXANT
               IF (AEXIST(J)) THEN
                  IF (REQBAS (I, J, REJECT, A, NA, B, NB)) THEN
                     IF (NBASLN.EQ.MXBASE) THEN
                        WRITE (MSGTXT, 1010) MXBASE
                        CALL MSGWRT (10)
                        IRET = 1
                        GO TO 999
                     ELSE
                        NBASLN = NBASLN + 1
                        BASLST(1, NBASLN) = I
                        BASLST(2, NBASLN) = J
                        END IF
                     END IF
                  END IF
 30            CONTINUE
            END IF
 40      CONTINUE
C
  999 RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TOO MANY BASELINE REQUESTS (MAX. ', I4, ')')
      END
      SUBROUTINE PASS (UVDATA, UVSCR, IRET)
C-----------------------------------------------------------------------
C   Make a pass through the data, processing each baseline in turn.
C
C   Inputs:
C      UVDATA  C*(*)    The name of the UVDATA object used to access the
C                       original data file.
C      UVSCR   C*(*)    The name of the UVDATA object used to access the
C                       scratch data file (which contains calibrated and
C                       edited data).
C
C  Outputs:
C      IRET    I        Status: 0 => pass completed with no errors
C                               anything else => error
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*)
      INTEGER   IRET
C
      INTEGER   CURBAS, IERR
C                                       Baseline being processed
C                                       1 <= CURBAS <= NBASELN
C
      CHARACTER ANAME1*8, ANAME2*8
C                                       Primary and secondary antenna
C                                       names for the baseline being
C                                       processed.
C
      CHARACTER ANTTBL*16
      PARAMETER (ANTTBL = 'Antenna table')
C                                       The name of the TABLE object
C                                       used to access the AN table.
C
      CHARACTER BSTBL*16
      PARAMETER (BSTBL = 'Solution table')
C                                       The name of the TABLE object
C                                       used to access the baseline
C                                       solution table
C
      CHARACTER BSMODE(4)*4
C                                       BS table mode strings.
C
      REAL      DATA(2)
      COMPLEX   CDATA(1)
      EQUIVALENCE (DATA, CDATA)
C                                       Base for dynamically allocated
C                                       data buffer
C
      LONGINT   DATOFF
C                                       Offset of dynamically allocated
C                                       data buffer from DATA
C
      INTEGER   DDIM(3)
C                                       Dimensions of dynamically
C                                       allocated data buffer (rate,
C                                       single-band delay, and multiband
C                                       delay).
C
      REAL      ACCEL(1)
C                                       Base for dynamically allocated
C                                       buffer holding peak amplitude,
C                                       phase, noise, rate, single-band
C                                       delay, and multiband delay for
C                                       each acceleration step
C
      LONGINT   ACCOFF
C                                       Offset of dynamically allocated
C                                       acceleration buffer
C
      INTEGER   MAXDP
C                                       Maximum number of data points to
C                                       be placed in cache
C
      REAL      PTIME(1)
      LONGINT   PTOFF
C                                       List of timestamps (dynamic
C                                       base plus offset)
C
      REAL      PDATA(1)
      LONGINT   PDOFF
C                                       List of phase data.  Each entry
C                                       contains the real and imaginary
C                                       components of a complex number
C                                       with a modulus that represents
C                                       the phase weight and an argument
C                                       that represents the phase.
C                                       (Dynamic base + offset)
C
      INTEGER   PIF(1), PCHAN(1), PPOL(1)
      LONGINT   PIOFF, PCOFF, PPOFF
C                                       List of IFs, channel and
C                                       polarizations (dynamic base +
C                                       offsets)
C
      INTEGER   TYPE, NWDS
C                                       Type code for AIPS object
C                                       attributes.
C
      INTEGER   DIM(3)
C                                       Dimensions for AIPS object
C                                       attributes.
C
      CHARACTER CDUMMY
C                                       Dummy character (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   DUMMY
C                                       Dummy number (used as a
C                                       place holder in AIPS object
C                                       system calls).
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
      INTEGER   MXASTP
      EXTERNAL  MXASTP
      REAL      MXSOLI
      EXTERNAL  MXSOLI
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BASELN.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'UVINFO.INC'
C
      DATA BSMODE /'INDE', 'VLBA', 'MK3 ', 'RATE'/
C-----------------------------------------------------------------------
C
C     Allocate dynamically sized buffers
C
      MAXDP = (NINT (MXSOLI () / MININT) + 1) * NAXIS(CHNIDX)
     *   * NAXIS(STKIDX)
      IF (IFIDX.GE.1) THEN
         MAXDP = MAXDP * NAXIS(IFIDX)
         END IF
      NWDS = (MAXDP - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'PASS', NWDS, PTIME, PTOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      CALL ZMEMRY ('GET ', 'PASS', 2 * NWDS, PDATA, PDOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      CALL ZMEMRY ('GET ', 'PASS', NWDS, PIF, PIOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      CALL ZMEMRY ('GET ', 'PASS', NWDS, PCHAN, PCOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      CALL ZMEMRY ('GET ', 'PASS', NWDS, PPOL, PPOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      CALL DATDIM (DDIM)
      NWDS = (2 * DDIM(1) * DDIM(2) * DDIM(3) - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'PASS', NWDS, DATA, DATOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
      NWDS = (6 * MXASTP() - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'PASS', NWDS, ACCEL, ACCOFF, IRET)
      IF (IRET.NE.0) THEN
         CALL MEMFAI
         GO TO 999
         END IF
C
C     Get the requested subarray number and set up an object through
C     which the corresponding AN table can be accessed.
C
      CALL SECGET (UVDATA, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SUBARR.EQ.0) SUBARR = 1
      CALL UV2TAB (UVDATA, ANTTBL, 'AN', SUBARR, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Open a baseline solution table for writing.
C
      CALL UV2TAB (UVDATA, BSTBL, 'BS', 0, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((MODE.EQ.1) .AND. (IFIDX.GT.0)) THEN
         BSIF = NAXIS(IFIDX)
      ELSE
         BSIF = 1
         END IF
      CALL OBSINI (BSTBL, 'WRIT', BSROW, BSMODE(MODE), BSIF, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Loop through baselines.  Inform the user as each baseline is
C     processed.
C
      DO 10 CURBAS = 1, NBASLN
         CALL ANTNFO (ANTTBL, SUBARR, BASLST(1, CURBAS), 'ANNAME',
     *      TYPE, DIM, DUMMY, ANAME1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ANTNFO (ANTTBL, SUBARR, BASLST(2, CURBAS), 'ANNAME',
     *      TYPE, DIM, DUMMY, ANAME2, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (MSGTXT, '(A, A, '' - '', A)') 'Processing baseline ',
     *      ANAME1(1:ITRIM(ANAME1)), ANAME2(1:ITRIM(ANAME2))
         CALL MSGWRT (4)
C
C        Process baseline
C
         DATOFF = (DATOFF - 1) / 2 + 1
         CALL DOBASE (CURBAS, UVDATA, UVSCR, BSTBL, CDATA(DATOFF+1),
     *      DDIM(1), DDIM(2), DDIM(3), ACCEL(ACCOFF+1), MXASTP(), MAXDP,
     *      PTIME(PTOFF+1), PDATA(PDOFF+1), PIF(PIOFF+1),
     *      PCHAN(PCOFF+1), PPOL(PPOFF+1), IRET)
         IF (IRET.NE.0) GO TO 999
C
   10    CONTINUE
C
C     Close BS table.
C
      CALL TABCLO (BSTBL, IRET)
      IF (IRET.NE.0) GO TO 999
C
      CALL ZMEMRY ('FRAL', 'PASS', MAXDP, PTIME, PTOFF, IERR)
C
  999 RETURN
C
      END
      SUBROUTINE DOBASE (BSNUM, UVDATA, UVSCR, BSTBL, DATA, TIMDIM,
     *   CHNDIM, IFDIM, ACCEL, ACCDIM, MAXDP, PTIME, PDATA, PIF, PCHAN,
     *   PPOL, IRET)
C-----------------------------------------------------------------------
C   Process the BSNUM-th baseline.
C
C   Inputs:
C      BSNUM    I        Baseline number (index into BASLST)
C      UVDATA   C*(*)    The name of the UVDATA object used for the
C                        input data set.
C      UVSCR    C*(*)    The name of the UVDATA object used for the
C                        scratch file.
C      BSTBL    C*(*)    The name of the TABLE object used for the
C                        baseline solution table.
C      TIMDIM   I        Time dimension of data buffer
C      CHNDIM   I        Channel dimension of data buffer
C      IFDIM    I        IF dimension of data buffer
C      ACCDIM   I        Dimension of acceleration buffer
C      MAXDP    I        Maximum number of data points in a solution
C
C   In/out:
C      DATA     C(TIMDIM, CHNDIM, IFDIM)
C                        Data buffer
C      ACCEL    R(6, ACCDIM)
C                        Acceleration buffer
C      PTIME    R(MAXDP) List of timestamps
C      PDATA    R(2, MAXDP)
C                        Phase data, stored as real/imaginary components
C                        scaled by data weight
C      PIF      I(MAXDP) List of IF numbers
C      PCHAN    I(MAXDP) List of channel numbers
C      PPOL     I(MAXDP) List of polarization numbers
C
C   Output:
C      IRET     I        Status: 0 => baseline processed
C                                anything else => error
C
C   Preconditions
C      1 <= BSNUM <= NBASLN
C      TIMDIM >= 1
C      CHNDIM >= 1
C      IFDIM >= 1
C      ACCDIM >= 1
C-----------------------------------------------------------------------
      INTEGER   BSNUM, TIMDIM, CHNDIM, IFDIM, ACCDIM
      CHARACTER UVDATA*(*), UVSCR*(*), BSTBL*(*)
      COMPLEX   DATA(TIMDIM, IFDIM, ACCDIM)
      REAL      ACCEL(6, ACCDIM)
      INTEGER   IRET
      INTEGER   MAXDP
      REAL      PTIME(MAXDP), PDATA(2, MAXDP)
      INTEGER   PIF(MAXDP), PCHAN(MAXDP), PPOL(MAXDP)
C
      INCLUDE 'UVINFO.INC'
C
      INTEGER   MAXRP
      PARAMETER (MAXRP = 16)
C                                       Maximum number of random
C                                       parameters.
      REAL      RP(MAXRP)
C                                       Random parameter array
C
      REAL      VIS(3 * MAXCIF)
C                                       Visibility buffer
C
      REAL      STOPTM
C                                       Next forced stop time from the
C                                       index table.
C
      CHARACTER NXTBL*12
      PARAMETER (NXTBL = 'Index table')
C
C                                       The name of the TABLE object
C                                       used to access the index table
C
      INTEGER   NXROW
C                                       The next row to be read from the
C                                       index table.
C
      LOGICAL   NXEXIS
C                                       Does the NX table exist?
C
      LOGICAL   DONE
C                                       Has the end of the data been
C                                       reached?
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Read first UV record
C
      CALL OUVOPN (UVSCR, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVREAD (UVSCR, RP, VIS, IRET)
      IF (IRET.NE.0) GO TO 999
C
C     Get the first forced stop time.
C
      CALL UV2TAB (UVDATA, NXTBL, 'NX', 1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABEXI (NXTBL, NXEXIS, IRET)
      IRET = 0
      STOPTM = -1.0
C                                       Error indicators from TABEXI are
C                                       not generally meaningful
      IF (NXEXIS) THEN
         CALL ONXINI (NXTBL, 'READ', NXROW, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL NXTSTP (NXTBL, NXROW, RP(TIMIDX), STOPTM, IDSOUR, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         STOPTM = 9999.9
         IDSOUR = 1
         END IF
C
C     Process the data
C
      DONE = .FALSE.
C                                       Simulated while loop
   10 IF (.NOT. DONE) THEN
         CALL DOSOLI (BSNUM, UVSCR, BSTBL, RP, MAXRP, VIS, STOPTM,
     *      NXTBL, NXROW, DONE, DATA, TIMDIM, CHNDIM, IFDIM, ACCEL,
     *      ACCDIM, MAXDP, PTIME, PDATA, PIF, PCHAN, PPOL, IRET)
         IF (IRET.NE.0) GO TO 999
         GO TO 10
         END IF
C
C     Close files ready for the next baseline pass.
C
      CALL OUVCLO (UVSCR, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NXEXIS) THEN
         CALL TABCLO (NXTBL, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL TABDES (NXTBL, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
C
      END
      SUBROUTINE NXTSTP (NXTBL, NXROW, CURTIM, STOPTM, IDSOUR, IRET)
C-----------------------------------------------------------------------
C   Find the next end-of-scan time that is greater than CURTIM.
C
C   Inputs:
C      NXTBL    C*(*)    The name of the TABLE object used to access
C                        the index table.  The table should be
C                        open if NXTSTP is called with CURTIM > STOPTM
C      CURTIM   R        Current time in days
C
C   In/Out
C      NXROW    I        The next NX table row to be read.
C      STOPTM   R        The next stop time in days.  If STOPTM >
C                        CURTIM when NXTSTP is called STOPTM will
C                        not be changed.
C      IDSOUR   I        Source ID
C
C   Output:
C      IRET     I        Status: 0 => STOPTM valid
C                                anything else => error
C-----------------------------------------------------------------------
      CHARACTER NXTBL*(*)
      INTEGER   NXROW, IDSOUR
      REAL      CURTIM, STOPTM
      INTEGER   IRET
C
      REAL      CTIME, DTIME
C                                       Center time and duration of a
C                                       scan in days.
C
      INTEGER   SUBARR, VSTART, VEND, FREQID, IDS
C                                       Subarray number,
C                                       start and end visibility numbers
C                                       and frqeuency ID for the scan
C-----------------------------------------------------------------------
      IRET = 0
C                                       Simulated while loop
   10 IF (STOPTM.LE.CURTIM) THEN
         CALL OTABNX (NXTBL, 'READ', NXROW, CTIME, DTIME, IDS,
     *      SUBARR, VSTART, VEND, FREQID, IRET)
         IF (IRET.EQ.0) THEN
            DTIME = DTIME + 2.E-7
            IF ((CTIME+DTIME/2.0.GT.CURTIM) .AND.
     *         (CTIME-DTIME/2.0.LE.CURTIM)) THEN
               STOPTM = CTIME + DTIME / 2.0
               IDSOUR = IDS
            ELSE
               GO TO 10
               END IF
            END IF
         END IF
C
  999 RETURN
C
      END
      SUBROUTINE DOSOLI (BSNUM, UVSCR, BSTBL, RP, MAXRP, VIS, STOPTM,
     *   NXTBL, NXROW, DONE, DATA, TIMDIM, CHNDIM, IFDIM, ACCEL, ACCDIM,
     *   MAXDP, PTIME, PDATA, PIF, PCHAN, PPOL, IRET)
C-----------------------------------------------------------------------
C   Accumulate data for one solution interval and search for the
C   residual delays, rate and acceleration at the centre of the solution
C   interval.  Data will be accumulated until either
C
C   - the data cache is filled,
C   - a solution interval has elapsed since the first time stamp,
C   - STOPTM is reached or
C   - the source ID changes.
C
C   RP and VIS should be initialized with the first visibility in the
C   solution interval when DOSOLI is called and will contain the first
C   visibility for the next solution interval if DONE is .FALSE. when
C   the routine returns.
C
C   Inputs:
C      BSNUM     I           Baseline number: 1 <= BSNUM <= NBASLN
C      UVSCR     C*(*)       Name of the UVDATA file used to access the
C                            scratch file holding the calibrated data.
C      BSTBL     C*(*)       Name of the TABLE object used to access the
C                            baseline solution table.
C      MAXRP     I           The maximum number of random parameters
C      NXTBL     C*(*)       The name of the TABLE object used to access
C                            the index table.
C      TIMDIM    I           Time dimension of data grid
C      CHNDIM    I           Channel dimension of data grid
C      IFGRID    I           IF dimension of data grid
C      ACCDIM    I           Dimension of acceleration buffer
C
C   In/out:
C      RP        R(MAXRP)    Random parameter buffer
C      VIS       R(3 * MAXCIF)
C                            Visibility buffer
C      STOPTM    R           Next forced break in days.
C      NXROW     I           Next index table row to be read
C      DATA      C(TIMDIM, CHNDIM, IFDIM)
C                            Data grid
C      ACCEL     R(6, ACCDIM)
C                            Acceleration buffer
C      PTIME    R(MAXDP)     List of timestamps
C      PDATA    R(2, MAXDP)
C                            Phase data, stored as real/imaginary
C                            components scaled by data weight
C      PIF      I(MAXDP)     List of IF numbers
C      PCHAN    I(MAXDP)     List of channel numbers
C      PPOL     I(MAXDP)     List of polarization numbers
C
C   Outputs
C      DONE      L           Has the end-of-data been reached?
C      IRET      I           Status: 0 => solution interval processed
C                                    anything else error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVINFO.INC'
C
      INTEGER   BSNUM
      CHARACTER UVSCR*(*), BSTBL*(*)
      INTEGER   MAXRP
      CHARACTER NXTBL*(*)
      REAL      RP(MAXRP), VIS(3 * MAXCIF), STOPTM
      INTEGER   NXROW, TIMDIM, CHNDIM, IFDIM, ACCDIM
      COMPLEX   DATA(TIMDIM, CHNDIM, IFDIM)
      REAL      ACCEL(6, ACCDIM)
      INTEGER   MAXDP
      REAL      PTIME(MAXDP), PDATA(2, MAXDP)
      INTEGER   PIF(MAXDP), PCHAN(MAXDP), PPOL(MAXDP)
      LOGICAL   DONE
      INTEGER   IRET
C
      INTEGER   NUMDP
C                                       Actual number of data points in
C                                       cache.
C                                       0 <= NUMDP <= MAXDP
C
      REAL      IBDATA(3 * MAXCIF, 2, MAXANT)
C                                       Raw data for the direct baseline
C                                       and intermediate baselines.  The
C                                       last index gives the
C                                       intermediate antenna (k) and the
C                                       second index distinguishes
C                                       between i-k (1) and k-j (2).
C                                       Raw data for i-j is stored in
C                                       IBDATA(:, 1, j).
C
      REAL      DTIME, DTIME2
C                                       Minimum time between adjacent
C                                       measurements (assumed to be
C                                       the time between samples) and a
C                                       temporary candidate value.
C
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCA1, ILOCA2, ILOCSA
C                                       Pointers to u, v, w, time,
C                                       baseline number, source number
C                                       and freqency ID in the random
C                                       parameter array (-1 if not
C                                       present).
C
      INTEGER   JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF
C                                       Axis numbers for complex,
C                                       Stokes, frequency, RA, dec
C                                       and IF (-1 if not present)
      INTEGER   INCS, INCF, INCIF
C                                       Data increments for Stokes,
C                                       frequency and IF
C
      REAL      T
C                                       Time being processed.
C
      REAL      TA
C                                       Timestamp of last accepted data
C                                       point.
C
      REAL      T0
C                                       Start time of solution interval.
C
      INTEGER   SRCHIX
C                                       Index of matching search
C                                       parameters.
C
      INTEGER   NUMIF
C                                       The number of IFs
C
      INTEGER   IIF, CHAN, POL
C                                       If channel and polarization
C                                       indices.
C
      INTEGER   VISIDX
C                                       Index into data buffers.
C
      INTEGER   A
C                                       Antenna number.
C
      REAL      W, W1, W2
C                                       Weights
C
      REAL      P, P1, P2
C                                       Phase
C
      LOGICAL   ENDSOL
C                                       Has the end of the solution
C                                       interval been reached?
C
      LOGICAL   WARNST
      SAVE      WARNST
C                                       Has the user been warned about
C                                       integration times shorter than
C                                       he claims?
C
      INTEGER   A1, A2
C                                       Antenna numbers for the current
C                                       data record.
C                                       1 <= A1 < A2 <= MAXANT
C
      INTEGER   DATSIZ
C                                       The number of words of data in
C                                       each record.
C                                       3 <= DATSIZ <= 3 * MAXCIF
C
      LOGICAL   GOOD
C                                       Does output record have data?
C
      INTEGER   I, J
C
      INTEGER   SRCHPM
      EXTERNAL  SRCHPM
C
      SAVE ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1,
     *   ILOCA2, ILOCSA, INCS, INCF, INCIF
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'BASELN.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'SEARCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA ILOCT  /-99/
      DATA WARNST /.FALSE./
C-----------------------------------------------------------------------
C
C     Set up data pointers (ILOCT will be -99 on the first call, the
C     pointers should be valid for all future calls).
C
      IF (ILOCT.EQ.-99) THEN
         CALL UVDPNT (UVSCR, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *      ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *      JLOCD, JLOCIF, INCS, INCF, INCIF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (IFIDX.NE.-1) THEN
         NUMIF = NAXIS(IFIDX)
      ELSE
         NUMIF =1
         END IF
      DATSIZ = 3 * NUMIF * NAXIS(STKIDX) * NAXIS(CHNIDX)
C
C     Initialize buffers and find the relevant search parameters.
C
      NUMDP = 0
      T = RP(ILOCT)
      T0 = RP(ILOCT)
      DTIME = 999.0
      DTIME2 = 999.0
      TA = -999.0
      IF (ILOCSU.GT.0) THEN
         SOURCE = RP(ILOCSU)
      ELSE
         SOURCE = IDSOUR
         END IF
      CALL RFILL (DATSIZ, 0.0, IBDATA(1, 1, BASLST(2, BSNUM)))
      IF (STACK) THEN
         DO 20 J = 1, MAXANT
            IF (AEXIST(J)) THEN
               DO 10 I = 1, 2
                  CALL RFILL (DATSIZ, 0.0, IBDATA(1, I, J))
   10             CONTINUE
               END IF
   20       CONTINUE
         END IF
      SRCHIX = SRCHPM (BASLST(1, BSNUM), BASLST(2, BSNUM), T0)
      DONE = .FALSE.
      ENDSOL = .FALSE.
C                                       Simulated while loop
   30 IF (.NOT. ENDSOL) THEN
C
C        Extract the antenna numbers and check whether the current data
C        point comes from a baseline we want.
C
         IF (ILOCB.GT.0) THEN
            A1 = INT (RP(ILOCB) / 256)
            A2 = INT (RP(ILOCB) - 256 * A1)
         ELSE
            A1 = RP(ILOCA1) + 0.1
            A2 = RP(ILOCA2) + 0.1
            END IF
         IF (ILOCSU.GT.0) THEN
            SOURCE = RP(ILOCSU)
         ELSE
            SOURCE = IDSOUR
            END IF
         IF ((A1.EQ.BASLST(1, BSNUM)) .AND.
     *      (A2.EQ.BASLST(2, BSNUM))) THEN
C                                       Exact match
            CALL RCOPY (DATSIZ, VIS, IBDATA(1, 1, A2))
            IF ((DTIME2.LT.DTIME) .AND. (DTIME2.GE.MININT)) THEN
               DTIME = DTIME2
            ELSE IF ((DTIME2.LT.MININT) .AND. (.NOT. WARNST)) THEN
               CALL STWARN (DTIME2, MININT)
               WARNST = .TRUE.
               END IF
            TA = T

         ELSE IF (STACK) THEN
C
C           Check for intermediate baselines
C
            IF (A1.EQ.BASLST(1, BSNUM)) THEN
C                                       have an i-k
               CALL RCOPY (DATSIZ, VIS, IBDATA(1, 1, A2))
               IF ((DTIME2.LT.DTIME) .AND. (DTIME2.GE.MININT)) THEN
                  DTIME = DTIME2
               ELSE IF ((DTIME2.LT.MININT) .AND. (.NOT. WARNST)) THEN
                  CALL STWARN (DTIME2, MININT)
                  WARNST = .TRUE.
                  END IF
               TA = T
            ELSE IF (A1.EQ.BASLST(2, BSNUM)) THEN
C                                       have a j-k - need to flip phase
               DO 40 VISIDX = 2, DATSIZ, 3
                  VIS(VISIDX) = -VIS(VISIDX)
   40             CONTINUE
               CALL RCOPY (DATSIZ, VIS, IBDATA(1, 2, A2))
               IF ((DTIME2.LT.DTIME) .AND. (DTIME2.GE.MININT)) THEN
                  DTIME = DTIME2
               ELSE IF ((DTIME2.LT.MININT) .AND. (.NOT. WARNST)) THEN
                  CALL STWARN (DTIME2, MININT)
                  WARNST = .TRUE.
                  END IF
               TA = T
            ELSE IF (A2.EQ.BASLST(1, BSNUM)) THEN
C                                       have a k-i - need to flip phase
               DO 50 VISIDX = 2, DATSIZ, 3
                  VIS(VISIDX) = -VIS(VISIDX)
   50             CONTINUE
               CALL RCOPY (DATSIZ, VIS, IBDATA(1, 1, A1))
               IF ((DTIME2.LT.DTIME) .AND. (DTIME2.GE.MININT)) THEN
                  DTIME = DTIME2
               ELSE IF ((DTIME2.LT.MININT) .AND. (.NOT. WARNST)) THEN
                  CALL STWARN (DTIME2, MININT)
                  WARNST = .TRUE.
                  END IF
               TA = T
            ELSE IF (A2.EQ.BASLST(2, BSNUM)) THEN
C                                       have a k-j
               CALL RCOPY (DATSIZ, VIS, IBDATA(1, 2, A1))
               IF ((DTIME2.LT.DTIME) .AND. (DTIME2.GE.MININT)) THEN
                  DTIME = DTIME2
               ELSE IF ((DTIME2.LT.MININT) .AND. (.NOT. WARNST)) THEN
                  CALL STWARN (DTIME2, MININT)
                  WARNST = .TRUE.
                  END IF
               TA = T
               END IF
            END IF
C
C        Read the next visibility
C
         CALL UVREAD (UVSCR, RP, VIS, IRET)
         IF (IRET.EQ.-1) THEN
C
C           Record that the end of data has been reached and place a
C           dummy timestamp in RP to force stacked data to be added in
C           before processing the last solution interval.  Also clear
C           error indicator.
C
            ENDSOL = .TRUE.
            DONE = .TRUE.
            RP(ILOCT) = T + 9999.0
            IRET = 0
         ELSE IF (IRET.NE.0) THEN
            GO TO 999
            END IF
C
         IF (RP(ILOCT).GT.T) THEN
C                                       New time stamp
C
C           Record the time difference between the new time stamp and
C           that of the last accepted data point.  This will become
C           the new estimate of the integration time if there is data
C           for a selected baseline at this time and it is shorter
C           than the previous estimate
C
            DTIME2 = RP(ILOCT) - TA
C
C           Move the direct baseline data, summing in any indirect data
C
            DO 90 POL = 1, NAXIS(STKIDX)
               IF (POLREQ(POLNUM(POL))) THEN
                  DO 80 IIF = 1, NUMIF
                     DO 70 CHAN = 1, NAXIS(CHNIDX)
                        NUMDP = NUMDP + 1
                        PDATA(1, NUMDP) = 0.0
                        PDATA(2, NUMDP) = 0.0
                        PTIME(NUMDP) = T
                        PPOL(NUMDP) = POLNUM(POL)
                        PIF(NUMDP) = IIF
                        PCHAN(NUMDP) = CHAN
                        VISIDX = (POL - 1) * INCS + (IIF - 1) * INCIF
     *                     + (CHAN - 1) * INCF
                        A2 = BASLST(2, BSNUM)
                        W = IBDATA(VISIDX + 3, 1, A2)
     *                     * (IBDATA(VISIDX + 1, 1, A2) ** 2
     *                     + IBDATA(VISIDX + 2, 1, A2) ** 2)
                        IF (W.GT.0.0) THEN
                           P = ATAN2 (IBDATA(VISIDX + 2, 1, A2),
     *                        IBDATA(VISIDX + 1, 1, A2))
                           PDATA(1, NUMDP) = PDATA(1, NUMDP) + W
     *                        * COS (P)
                           PDATA(2, NUMDP) = PDATA(2, NUMDP) + W
     *                        * SIN (P)
                           END IF
                        IF (STACK) THEN
                           DO 60 A = 1, MAXANT
                              IF (AEXIST(A)) THEN
                                 W1 = IBDATA(VISIDX + 3, 1, A)
     *                              * (IBDATA(VISIDX + 1, 1, A) ** 2
     *                              + IBDATA(VISIDX + 2, 1, A) ** 2)
                                 W2 = IBDATA(VISIDX + 3, 2, A)
     *                              * (IBDATA(VISIDX + 1, 2, A) ** 2
     *                           + IBDATA(VISIDX + 2, 2, A) ** 2)
                                 IF ((W1.GT.0.0) .AND. (W2.GT.0.0))
     *                           THEN
                                    P1 =
     *                                 ATAN2 (IBDATA(VISIDX + 2, 1, A),
     *                                 IBDATA(VISIDX + 1, 1, A))
                                    P2 =
     *                                 ATAN2 (IBDATA(VISIDX + 2, 2, A),
     *                                 IBDATA(VISIDX + 1, 1, A))
                                    W = (W1 * W2) ** 2 /
     *                                 (W1 ** 2 + W2 ** 2)
                                    PDATA(1, NUMDP) = PDATA(1, NUMDP)
     *                                 + W * COS (P1 + P2)
                                    PDATA(2, NUMDP) = PDATA(2, NUMDP)
     *                                 + W * SIN (P1 + P2)
                                    END IF
                                 END IF
   60                         CONTINUE
                           END IF
C
C                       Roll back the data pointer to save space if
C                       the current data point has no weight.
C
                        W = PDATA(1, NUMDP) ** 2 + PDATA(2, NUMDP)
                        IF (W.EQ.0.0) THEN
                           NUMDP = NUMDP - 1
                           END IF
   70                   CONTINUE
   80                CONTINUE
                  END IF
   90          CONTINUE
C
C           Clear buffer
C
            CALL RFILL (DATSIZ, 0.0, IBDATA(1, 1, BASLST(2, BSNUM)))
            IF (STACK) THEN
               DO 110 J = 1, MAXANT
                  IF (AEXIST(J)) THEN
                     DO 100 I = 1, 2
                        CALL RFILL (DATSIZ, 0.0, IBDATA(1, I, J))
  100                   CONTINUE
                     END IF
  110             CONTINUE
               END IF
C
C        Check whether a time limit has been reached
C
            IF (RP(ILOCT).GT.STOPTM) THEN
               IF (.NOT. DONE) THEN
C
C           Get next hard stop time
C
                  CALL NXTSTP (NXTBL, NXROW, RP(ILOCT), STOPTM, IDSOUR,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  ENDSOL = .TRUE.
                  END IF
            ELSE IF (RP(ILOCT).GT.(T0 + SRCHSI(SRCHIX))) THEN
C                                       End of solution interval.
               ENDSOL = .TRUE.
            ELSE IF ((NUMDP + DATSIZ / 3).GT.MAXDP) THEN
C                                       There is no room for another
C                                       time in the buffer.  This is
C                                       conservative in that it doesn't
C                                       take polarization subsetting or
C                                       bad data into account.
C                                       This should not happen unless
C                                       MAXDP was calculated
C                                       incorrectly.
               ENDSOL = .TRUE.
            ELSE IF (ILOCSU.GT.0) THEN
C                                       Source changed.
               IF (RP(ILOCSU).NE.SOURCE) THEN
                  ENDSOL = .TRUE.
                  END IF
               END IF
C
            END IF
         T = RP(ILOCT)
         GO TO 30
         END IF
C
      IF (NUMDP.GT.0) THEN
C
C        Do solution for each selected polarization.
C
         DO 210 POL = 1, MAXPOL
            IF (POLREQ(POL)) THEN
C
C              Fill in basic information in the BS record
C
               CTIME = (PTIME(NUMDP) + PTIME(1)) / 2.0
               TIMINT = PTIME(NUMDP) - PTIME(1)
               BASELN(1) = BASLST(1, BSNUM)
               BASELN(2) = BASLST(2, BSNUM)
               STOKES = POLCOD(POL)
               IF (MODE.EQ.1) THEN
C
C                 Solve independently for each IF
C
                  DO 200 IIF = 1, NUMIF
                     CALL SOLIND (POL, IIF, DTIME, SRCHIX, DATA, TIMDIM,
     *                  CHNDIM, IFDIM, ACCEL, ACCDIM, NUMDP, PTIME,
     *                  PDATA, PIF, PCHAN, PPOL, IRET)
                     IF (IRET.NE.0) GO TO 999
  200                CONTINUE
C
C                 Blank multiband delay
C
                  RMBD = FBLANK
                  MBDERR = FBLANK
                  MBDAMB = FBLANK
               ELSE IF (MODE.EQ.2) THEN
C
C                 Solve for a unified delay
C
                  CALL SOLVLB (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *               IFDIM, ACCEL, ACCDIM, NUMDP, PTIME, PDATA, PIF,
     *               PCHAN, PPOL, IRET)
                  IF (IRET.NE.0) GO TO 999
                  RMBD   = FBLANK
                  MBDERR = FBLANK
                  MBDAMB = FBLANK
               ELSE IF (MODE.EQ.3) THEN
C
C                 Solve for multi and single-band delays
C
                  CALL SOLMK3 (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *               IFDIM, ACCEL, ACCDIM, NUMDP, PTIME, PDATA, PIF,
     *               PCHAN, PPOL, IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
C
C                 Solve for rate only
C
                  CALL SOLRAT (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *               IFDIM, ACCEL, ACCDIM, NUMDP, PTIME, PDATA, PPOL,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  RMBD      = FBLANK
                  MBDERR    = FBLANK
                  MBDAMB    = FBLANK
                  SBDAMB    = FBLANK
                  END IF
               IF (MODE.EQ.1) THEN
                  GOOD = .FALSE.
                  DO 205 IIF = 1, NUMIF
                     IF (VAMP(IIF).NE.FBLANK) THEN
                        GOOD = .TRUE.
                        END IF
  205                CONTINUE
               ELSE
                  GOOD = VAMP(1).NE.FBLANK
                  END IF
               IF (GOOD) THEN
                  IF (MODE.EQ.1) THEN
                     CALL BLREPT (CTIME, POLLAB(-STOKES), NUMIF, RMBD,
     *                    RSBD, RRATE, RACCEL)
                  ELSE
                     CALL BLREPT (CTIME, POLLAB(-STOKES), 1, RMBD,
     *                    RSBD, RRATE, RACCEL)
                     END IF
                  CALL OTABBS (BSTBL, 'WRIT', BSROW, BSIF, CTIME,
     *               TIMINT, BASELN, SUBARR, STOKES, SOURCE, VAMP, SAMP,
     *               RMBD, MBDERR, MBDAMB, RSBD, SBDERR, SBDAMB, RRATE,
     *               RTERR, RTAMB, RACCEL, ACCERR, RPHASE, PHSERR, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
 210        CONTINUE
         END IF
C
  999 RETURN
      END
      SUBROUTINE SOLIND (POL, IIF, DTIME, SRCHIX, DATA, TIMDIM,
     *                  CHNDIM, IFDIM, ACCBUF, ACCDIM, NUMDP, PTIME,
     *                  PDATA, PIF, PCHAN, PPOL, IRET)
C-----------------------------------------------------------------------
C   Search for fringes in the given polarization and IF.
C
C   Inputs:
C      POL       I            Polarization number
C      IIF       I            IF number
C      DTIME     R            Time spacing in days.
C      SRCHIX    I            Index of search parameters
C      CHNDIM    I            Channel dimension of data grid
C      IFDIM     I            IF dimension of data grid
C      ACCDIM    I            Dimension of acceleration buffer
C      NUMDP     I            Number of data points in solution
C
C   In/Out:
C      DATA      C(TIMDIM, CHNDIM, IFDIM)
C                             Data grid
C      ACCBUF    R(6, ACCDIM) Acceleration buffer
C      PTIME     R(MAXDP)     List of timestamps
C      PDATA     R(2, MAXDP)
C                            Phase data, stored as real/imaginary
C                            components scaled by data weight
C      PIF       I(MAXDP)     List of IF numbers
C      PCHAN     I(MAXDP)     List of channel numbers
C      PPOL      I(MAXDP)     List of polarization numbers
C
C   Output
C      IRET      I            Return code: 0 => OK
C                                          anything else: unrecoverable
C                                                         error
C-----------------------------------------------------------------------
      INTEGER   POL, IIF
      REAL      DTIME
      INTEGER   SRCHIX, TIMDIM, CHNDIM, IFDIM, ACCDIM, IRET
      COMPLEX   DATA(TIMDIM, CHNDIM, IFDIM)
      REAL      ACCBUF(6, ACCDIM)
      INTEGER   NUMDP
      REAL      PTIME(*), PDATA(2, *)
      INTEGER   PIF(*), PCHAN(*), PPOL(*)
C
      REAL      SECPDY
      PARAMETER (SECPDY = 24.0 * 60.0 * 60.0)
C                                       Seconds per day
C
      INTEGER   NUMT
C                                       Number of times in solution
C                                       interval
C
      INTEGER   NT
C                                       Length of delay/rate transform
C
      INTEGER   NF
C                                       Length of frequency/delay
C                                       transform
C
      INTEGER   MR
C                                       Number of rate cell to search
C
      INTEGER   MD
C                                       Number of delay cells to search
C
      REAL      DR
C                                       Rate cell increment
C
      REAL      DD
C                                       Delay cell increment
C
      REAL      ACCEL
C                                       Current acceleration value
C
      INTEGER   NUMACC
C                                       Number of accelerations searched
C
      INTEGER   TIMIND
C                                       Time index
C
      REAL      PHCOR
C                                       Phase correction
C
      REAL      SUMAMP
C                                       Sum of amplitudes
C
      REAL      ADJUST
C                                       Acceleration interpolation
C                                       adjustment
C
      REAL      X(3), Y(3)
C                                       Interpolation arrays
C
      INTEGER   IT(4)
C                                       Time broken down into integer
C                                       components
C
      LOGICAL   RTGOOD, SBGOOD, MBGOOD
      INTEGER   I, J
      CHARACTER CDUMMY
C
      INTEGER   NXPOW2
      EXTERNAL  NXPOW2
      DOUBLE PRECISION RFREQ
      EXTERNAL  RFREQ
C
      INCLUDE 'INCS:PFFT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'SEARCH.INC'
      INCLUDE 'UVINFO.INC'
C
      DATA X / -1.0, 0.0, +1.0 /
C-----------------------------------------------------------------------
      IRET = 0
C
      RTAMB  = 1.0 / (DTIME * SECPDY)
      SBDAMB = 1.0 / (RFREQ (2, IIF) - RFREQ (1, IIF))
      NUMT   = NINT ((PTIME(NUMDP) - PTIME(1)) / DTIME) + 1
      NT     = 2**RBLOAT * NXPOW2 (NUMT)
      NF     = 2**DBLOAT * NXPOW2 (NAXIS(CHNIDX))
      DR     = RTAMB / NT
      DD     = SBDAMB / NF
C
C     Blank output solutions:
C
      CALL BSBLNK (IIF)
C
C     Check that there is more than one frequency channel so that a
C     delay search is meaningful:
C
      IF (NAXIS(CHNIDX).LE.1) THEN
         IRET = 1
         MSGTXT = 'SOLIND: NEED MORE THAN ONE CHANNEL FOR DELAY SEARCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
C     Check that there are enough integrations in the solution interval:
C
      IF (NUMT.LE.1) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) IIF, POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         MSGTXT = '    (insufficient data)'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Check FFT limit:
C
      IF (NF.GT.FFTMAX) THEN
         IRET = 2
         MSGTXT = 'CHANNEL FFT EXCEEDS LIMIT FOR THIS SYSTEM'
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NT.GT.FFTMAX) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) IIF, POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Calculate rate window size in cells:
C
      IF (SRCHWN(2, 3, SRCHIX).GT.0.0) THEN
         MR = NINT (SRCHWN(2, 3, SRCHIX) / DR + 0.5)
         MR = MAX (1, MR)
         MR = MIN (MR, NT)
      ELSE
         MR = NT
         END IF
C
C     Calculate delay window in cells:
C
      IF (SRCHWN(2, 2, SRCHIX).GT.0.0) THEN
         MD = NINT (SRCHWN(2, 2, SRCHIX) / DD + 0.5)
         MD = MAX (1, MD)
         MD = MIN (MD, NF)
      ELSE
         MD = NF
         END IF
C
C     Search accelerations:
C
      ACCEL = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
      NUMACC = 0
 30   IF (ACCEL
     *  .LE.(SRCHWN(1, 4, SRCHIX) + SRCHWN(2, 4, SRCHIX) / 2.0)) THEN
         NUMACC = NUMACC + 1
C
C        Accumulate data:
C
         DO 20 J = 1, NF
            DO 10 I = 1, NT
               DATA(I, J, 1) = CMPLX (0.0, 0.0)
 10            CONTINUE
 20         CONTINUE
         SUMAMP = 0.0
         DO 40 I = 1, NUMDP
            IF ((PIF(I).EQ.IIF) .AND. (PPOL(I).EQ.POL)) THEN
               TIMIND = NINT ((PTIME(I) - PTIME(1)) / DTIME) + 1
               PHCOR  = -TWOPI
     *            * (SRCHWN(1, 2, SRCHIX)
     *               * (RFREQ (PCHAN(I), IIF) - UVFREQ)
     *               + SRCHWN(1, 3, SRCHIX) * SECPDY
     *               * (PTIME(I) - CTIME)
     *               + 0.5 * ACCEL * (SECPDY * (PTIME(I) - CTIME))**2)
               DATA(TIMIND, PCHAN(I), 1) =  CMPLX (COS (PHCOR),
     *                                             SIN (PHCOR))
     *            * CMPLX (PDATA(1, I), PDATA(2, I))
               SUMAMP = SUMAMP + SQRT (PDATA(1, I)**2 + PDATA(2, I)**2)
               END IF
 40         CONTINUE
C
C        Trap the case where no data was available for the current
C        polarization and IF:
C
         IF (SUMAMP.LE.0.0) THEN
            WRITE (MSGTXT, 1000) IIF, POLLAB(POL), IT(1), IT(2), IT(3),
     *         IT(4)
            CALL MSGWRT (6)
            MSGTXT = '    (insufficient data)'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C
         CALL FRSRCI (NT, NF, 1, MR, MD, 1, DATA, TIMDIM, CHNDIM,
     *      ACCBUF(4, NUMACC), ACCBUF(5, NUMACC), ACCBUF(6, NUMACC),
     *      ACCBUF(1, NUMACC), ACCBUF(2, NUMACC), ACCBUF(3, NUMACC),
     *      RTGOOD, SBGOOD, MBGOOD, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SOLIND: FATAL ERROR DETECTED IN FRSRCI'
            CALL MSGWRT (9)
            GO TO 999
            END IF
         IF (SRCHWN(2, 4, SRCHIX).GT.0.0) THEN
            ACCEL = ACCEL + SRCHDA(SRCHIX)
            GO TO 30
            END IF
         END IF
C
C     Find acceleration with best signal:
C
      J = 1
      DO 50 I = 1, NUMACC
         IF (ACCBUF(1, I).GT.ACCBUF(1, J)) THEN
            J = I
            END IF
 50      CONTINUE
C
C     Check good solution criteria
C
      IF ((ACCBUF(1, J) / ACCBUF(3, J)).LT.MINSNR) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1030) IIF, POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE IF ((ACCBUF(1, J) / SUMAMP).LT.CTHRSH) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1031) IIF, POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         RSBD(IIF)   = -ACCBUF(5, J) * DD + SRCHWN(1, 2, SRCHIX)
         SBDERR(IIF) = 0.5 * DD
         RRATE(IIF)  = -ACCBUF(4, J) * DR + SRCHWN(1, 3, SRCHIX)
         RTERR(IIF)  = 0.5 * DR
         RACCEL(IIF) = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
     *      + SRCHDA(SRCHIX) * (J - 1)
         ACCERR(IIF) = MAX (0.0, 0.5 * SRCHDA(SRCHIX))
         RPHASE(IIF) = RAD2DG * ACCBUF(2, J)
     *      - 360.0 * RSBD(IIF)
     *              * (RFREQ (NAXIS(CHNIDX), IIF) - RFREQ (1 , IIF))
     *                 / 2.0
         PHSERR(IIF) = RAD2DG * ACCBUF(3, J) / ACCBUF(1, J)
         VAMP(IIF)   = ACCBUF(1, J)
         SAMP(IIF)   = SUMAMP
C
C        Interpolate acceleration if not at edge of search range:
C
         IF ((1.LT.J) .AND. (J.LT.NUMACC)) THEN
            Y(1) = ACCBUF(1, J-1)
            Y(2) = ACCBUF(1, J)
            Y(3) = ACCBUF(1, J+1)
            CALL SVANDT (2, X, Y)
            ADJUST = - 0.5 * Y(2) / Y(3)
            RACCEL(IIF) = RACCEL(IIF) + ADJUST * SRCHDA(SRCHIX)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Skipped IF ', I3, ' ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2,':', I2.2)
 1001 FORMAT ('    (more than ', I6, ' FFT points)')
 1030 FORMAT ('Rejected IF ', I3, ' ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low SNR)')
 1031 FORMAT ('Rejected IF ', I3, ' ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low coherence)')
      END
      SUBROUTINE SOLVLB (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *                   IFDIM, ACCBUF, ACCDIM, NUMDP, PTIME, PDATA,
     *                   PIF, PCHAN, PPOL, IRET)
C-----------------------------------------------------------------------
C   Search for fringes in the given polarization assuming a single
C   delay applies to all channels and IFs.
C
C   Inputs:
C      POL       I            Polarization number
C      DTIME     R            Time spacing in days.
C      SRCHIX    I            Index of search parameters
C      TIMDIM    I            Time dimension of data grid
C      CHNDIM    I            Channel dimension of data grid
C      IFDIM     I            IF dimension of data grid
C      ACCDIM    I            Dimension of acceleration buffer
C      NUMDP     I            Number of data points in solution
C
C   In/Out:
C      DATA      C(TIMDIM, CHNDIM, IFDIM)
C                             Data grid
C      ACCBUF    R(6, ACCDIM) Acceleration buffer
C      PTIME     R(MAXDP)     List of timestamps
C      PDATA     R(2, MAXDP)
C                            Phase data, stored as real/imaginary
C                            components scaled by data weight
C      PIF       I(MAXDP)     List of IF numbers
C      PCHAN     I(MAXDP)     List of channel numbers
C      PPOL      I(MAXDP)     List of polarization numbers
C
C   Output
C      IRET      I            Return code: 0 => OK
C                                          anything else: unrecoverable
C                                                         error
C-----------------------------------------------------------------------
      INTEGER   POL
      REAL      DTIME
      INTEGER   SRCHIX, TIMDIM, CHNDIM, IFDIM, ACCDIM, IRET
      COMPLEX   DATA(TIMDIM, CHNDIM, IFDIM)
      REAL      ACCBUF(6, ACCDIM)
      INTEGER   NUMDP
      REAL      PTIME(*), PDATA(2, *)
      INTEGER   PIF(*), PCHAN(*), PPOL(*)
C
      REAL      SECPDY
      PARAMETER (SECPDY = 24.0 * 60.0 * 60.0)
C                                       Seconds per day
C
      INTEGER   NUMT
C                                       Number of times in solution
C                                       interval
C
      INTEGER   NUMIF
C                                       Number of IFs in data
C
      INTEGER   NUMF
C                                       Total number of frequencies
C
      INTEGER   NT
C                                       Length of delay/rate transform
C
      INTEGER   NF
C                                       Length of frequency/delay
C                                       transform
C
      INTEGER   MR
C                                       Number of rate cell to search
C
      INTEGER   MD
C                                       Number of delay cells to search
C
      REAL      DF
C                                       Frequency increment
C
      REAL      DR
C                                       Rate cell increment
C
      REAL      DD
C                                       Delay cell increment
C
      REAL      ACCEL
C                                       Current acceleration value
C
      INTEGER   NUMACC
C                                       Number of accelerations searched
C
      INTEGER   TIMIND
C                                       Time index
C
      INTEGER   FRQIDX
C                                       Frequency index
C
      REAL      PHCOR
C                                       Phase correction
C
      REAL      SUMAMP
C                                       Sum of amplitudes
C
      REAL      ADJUST
C                                       Acceleration interpolation
C                                       adjustment
C
      REAL      X(3), Y(3)
C                                       Interpolation arrays
C
      INTEGER   IT(4)
C                                       Time broken down into integer
C                                       components
C
      LOGICAL   RTGOOD, SBGOOD, MBGOOD
      INTEGER   I, J
      CHARACTER CDUMMY
C
      INTEGER   NXPOW2
      EXTERNAL  NXPOW2
      DOUBLE PRECISION RFREQ
      EXTERNAL  RFREQ
C
      INCLUDE 'INCS:PFFT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'SEARCH.INC'
      INCLUDE 'UVINFO.INC'
C
      DATA X / -1.0, 0.0, +1.0 /
C-----------------------------------------------------------------------
      IRET = 0
C
      IF (IFIDX.GE.1) THEN
         NUMIF = NAXIS(IFIDX)
      ELSE
         NUMIF = 1
         END IF
C
      DF     = RFREQ (2, 1) - RFREQ (1, 1)
      RTAMB  = 1.0 / (DTIME * SECPDY)
      SBDAMB = 1.0 / DF
      NUMT   = NINT ((PTIME(NUMDP) - PTIME(1)) / DTIME) + 1
      NUMF   = NINT ((RFREQ (NAXIS(CHNIDX), NUMIF) - RFREQ (1, 1)) / DF)
     *         + 1
      NT     = 2**RBLOAT * NXPOW2 (NUMT)
      NF     = 2**DBLOAT * NXPOW2 (NUMF)
      DR     = RTAMB / NT
      DD     = SBDAMB / NF
C
C     Blank output solutions:
C
      CALL BSBLNK (1)
C
C     Check that there is more than one frequency channel so that a
C     delay search is meaningful:
C
      IF (NUMF.LE.1) THEN
         IRET = 1
         MSGTXT = 'SOLIND: NEED MORE THAN ONE CHANNEL FOR DELAY SEARCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
C     Check that there are enough integrations in the solution interval:
C
      IF (NUMT.LE.1) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         MSGTXT = '    (insufficient data)'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Check FFT limit:
C
      IF (NF.GT.FFTMAX) THEN
         IRET = 2
         MSGTXT = 'CHANNEL FFT EXCEEDS LIMIT FOR THIS SYSTEM'
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NT.GT.FFTMAX) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Calculate rate window size in cells:
C
      IF (SRCHWN(2, 3, SRCHIX).GT.0.0) THEN
         MR = NINT (SRCHWN(2, 3, SRCHIX) / DR + 0.5)
         MR = MAX (1, MR)
         MR = MIN (MR, NT)
      ELSE
         MR = NT
         END IF
C
C     Calculate delay window in cells:
C
      IF (SRCHWN(2, 2, SRCHIX).GT.0.0) THEN
         MD = NINT (SRCHWN(2, 2, SRCHIX) / DD + 0.5)
         MD = MAX (1, MD)
         MD = MIN (MD, NF)
      ELSE
         MD = NF
         END IF
C
C     Search accelerations:
C
      ACCEL = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
      NUMACC = 0
 30   IF (ACCEL
     *  .LE.(SRCHWN(1, 4, SRCHIX) + SRCHWN(2, 4, SRCHIX) / 2.0)) THEN
         NUMACC = NUMACC + 1
C
C        Accumulate data:
C
         DO 20 J = 1, NF
            DO 10 I = 1, NT
               DATA(I, J, 1) = CMPLX (0.0, 0.0)
 10            CONTINUE
 20         CONTINUE
         SUMAMP = 0.0
         DO 40 I = 1,NUMDP
            IF (PPOL(I).EQ.POL) THEN
               TIMIND = NINT ((PTIME(I) - PTIME(1)) / DTIME) + 1
               FRQIDX = NINT ((RFREQ (PCHAN(I), PIF(I)) - RFREQ (1, 1))
     *                         / DF) + 1
               PHCOR  = -TWOPI
     *            * (SRCHWN(1, 2, SRCHIX)
     *               * (RFREQ (PCHAN(I), PIF(I)) - UVFREQ)
     *               + SRCHWN(1, 3, SRCHIX) * SECPDY
     *               * (PTIME(I) - CTIME)
     *               + 0.5 * ACCEL * (SECPDY * (PTIME(I) - CTIME))**2)
               DATA(TIMIND, FRQIDX, 1) =  CMPLX (COS (PHCOR),
     *                                             SIN (PHCOR))
     *            * CMPLX (PDATA(1, I), PDATA(2, I))
               SUMAMP = SUMAMP + SQRT (PDATA(1, I)**2 + PDATA(2, I)**2)
               END IF
 40         CONTINUE
C
C        Trap the case where no data was available for the current
C        polarization and IF:
C
         IF (SUMAMP.LE.0.0) THEN
            WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3),
     *         IT(4)
            CALL MSGWRT (6)
            MSGTXT = '    (insufficient data)'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C
         CALL FRSRCI (NT, NF, 1, MR, MD, 1, DATA, TIMDIM, CHNDIM,
     *      ACCBUF(4, NUMACC), ACCBUF(5, NUMACC), ACCBUF(6, NUMACC),
     *      ACCBUF(1, NUMACC), ACCBUF(2, NUMACC), ACCBUF(3, NUMACC),
     *      RTGOOD, SBGOOD, MBGOOD, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SOLIND: FATAL ERROR DETECTED IN FRSRCI'
            CALL MSGWRT (9)
            GO TO 999
            END IF
         IF (SRCHWN(2, 4, SRCHIX).GT.0.0) THEN
            ACCEL = ACCEL + SRCHDA(SRCHIX)
            GO TO 30
            END IF
         END IF
C
C     Find acceleration with best signal:
C
      J = 1
      DO 50 I = 1, NUMACC
         IF (ACCBUF(1, I).GT.ACCBUF(1, J)) THEN
            J = I
            END IF
 50      CONTINUE
C
C     Check good solution criteria
C
      IF ((ACCBUF(1, J) / ACCBUF(3, J)).LT.MINSNR) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1030) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE IF ((ACCBUF(1, J) / SUMAMP).LT.CTHRSH) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1031) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         RSBD(1)   = -ACCBUF(5, J) * DD + SRCHWN(1, 2, SRCHIX)
         SBDERR(1) = 0.5 * DD
         RRATE(1)  = -ACCBUF(4, J) * DR + SRCHWN(1, 3, SRCHIX)
         RTERR(1)  = 0.5 * DR
         RACCEL(1) = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
     *      + SRCHDA(SRCHIX) * (J - 1)
         ACCERR(1) = MAX (0.0, 0.5 * SRCHDA(SRCHIX))
         RPHASE(1) = RAD2DG * ACCBUF(2, J)
     *      - 360.0 * RSBD(1)
     *              * ((RFREQ (NAXIS(CHNIDX), NUMIF) + RFREQ (1 , 1))
     *                 / 2.0 - UVFREQ)
         PHSERR(1) = RAD2DG * ACCBUF(3, J) / ACCBUF(1, J)
         VAMP(1)   = ACCBUF(1, J)
         SAMP(1)   = SUMAMP
C
C        Interpolate acceleration if not at edge of search range:
C
         IF ((1.LT.J) .AND. (J.LT.NUMACC)) THEN
            Y(1) = ACCBUF(1, J-1)
            Y(2) = ACCBUF(1, J)
            Y(3) = ACCBUF(1, J+1)
            CALL SVANDT (2, X, Y)
            ADJUST = - 0.5 * Y(2) / Y(3)
            RACCEL(1) = RACCEL(1) + ADJUST * SRCHDA(SRCHIX)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Skipped ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2,':', I2.2)
 1001 FORMAT ('    (more than ', I6, ' FFT points)')
 1030 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low SNR)')
 1031 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low coherence)')
      END
      SUBROUTINE SOLMK3 (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *                   IFDIM, ACCBUF, ACCDIM, NUMDP, PTIME, PDATA,
     *                   PIF, PCHAN, PPOL, IRET)
C-----------------------------------------------------------------------
C   Search for fringes in the given polarization assuming single-band
C   delays are the same for all IFs but different from the multiband
C   delay.
C
C   Inputs:
C      POL       I            Polarization number
C      DTIME     R            Time spacing in days.
C      SRCHIX    I            Index of search parameters
C      TIMDIM    I            Time dimension of data grid
C      CHNDIM    I            Channel dimension of data grid
C      IFDIM     I            IF dimension of data grid
C      ACCDIM    I            Dimension of acceleration buffer
C      NUMDP     I            Number of data points in solution
C
C   In/Out:
C      DATA      C(TIMDIM, CHNDIM, IFDIM)
C                             Data grid
C      ACCBUF    R(6, ACCDIM) Acceleration buffer
C      PTIME     R(MAXDP)     List of timestamps
C      PDATA     R(2, MAXDP)
C                            Phase data, stored as real/imaginary
C                            components scaled by data weight
C      PIF       I(MAXDP)     List of IF numbers
C      PCHAN     I(MAXDP)     List of channel numbers
C      PPOL      I(MAXDP)     List of polarization numbers
C
C   Output
C      IRET      I            Return code: 0 => OK
C                                          anything else: unrecoverable
C                                                         error
C-----------------------------------------------------------------------
      INTEGER   POL
      REAL      DTIME
      INTEGER   SRCHIX, TIMDIM, CHNDIM, IFDIM, ACCDIM, IRET
      COMPLEX   DATA(TIMDIM, CHNDIM, IFDIM)
      REAL      ACCBUF(6, ACCDIM)
      INTEGER   NUMDP
      REAL      PTIME(*), PDATA(2, *)
      INTEGER   PIF(*), PCHAN(*), PPOL(*)
C
      INCLUDE 'INCS:PUVD.INC'
C
      REAL      SECPDY
      PARAMETER (SECPDY = 24.0 * 60.0 * 60.0)
C                                       Seconds per day
C
      INTEGER   NUMT
C                                       Number of times in solution
C                                       interval
C
      INTEGER   NUMIF
C                                       Number of IFs in data
C
      INTEGER   NUMF
C                                       Total number of frequencies
C
      INTEGER   NT
C                                       Length of delay/rate transform
C
      INTEGER   NF
C                                       Length of frequency/single-band
C                                       delay transform
C
      INTEGER   NIF
C                                       Length of IF/multiband delay
C                                       transform
C
      INTEGER   MR
C                                       Number of rate cell to search
C
      INTEGER   MSBD
C                                       Number of single-band delay
C                                       cells to search
C
      INTEGER   MMBD
C                                       Number of multiband delay cells
C                                       to search
C
      REAL      DF
C                                       Frequency increment
C
      REAL      DIF
C                                       IF increment
C
      REAL      DR
C                                       Rate cell increment
C
      REAL      DSBD
C                                       Delay cell increment
C
      REAL      DMBD
C                                       Multiband delay cell increment
C
      DOUBLE PRECISION IFFREQ(MAXIF)
C                                       IF lower band-edges
C
      DOUBLE PRECISION LOFREQ, HIFREQ
C                                       Highest and lowest IF
C                                       frequencies
C
      REAL      ACCEL
C                                       Current acceleration value
C
      INTEGER   NUMACC
C                                       Number of accelerations searched
C
      INTEGER   TIMIND
C                                       Time index
C
      INTEGER   FRQIDX
C                                       Frequency index
C
      INTEGER   IIFIDX
C                                       IF index
C
      REAL      PHCOR
C                                       Phase correction
C
      REAL      SUMAMP
C                                       Sum of amplitudes
C
      REAL      ADJUST
C                                       Acceleration interpolation
C                                       adjustment
C
      REAL      X(3), Y(3)
C                                       Interpolation arrays
C
      INTEGER   IT(4)
C                                       Time broken down into integer
C                                       components
C
      LOGICAL   RTGOOD, SBGOOD, MBGOOD
      INTEGER   I, J, K
      CHARACTER CDUMMY
C
      INTEGER   NXPOW2
      EXTERNAL  NXPOW2
      DOUBLE PRECISION RFREQ
      EXTERNAL  RFREQ
      DOUBLE PRECISION IFINC
      EXTERNAL  IFINC
C
      INCLUDE 'INCS:PFFT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'SEARCH.INC'
      INCLUDE 'UVINFO.INC'
C
      DATA X / -1.0, 0.0, +1.0 /
C-----------------------------------------------------------------------
      IRET   = 0
      RMBD   = FBLANK
      MBDERR = FBLANK
C
      IF (IFIDX.GE.1) THEN
         NUMIF = NAXIS(IFIDX)
      ELSE
         NUMIF = 1
         END IF
C
C     Calculate increments and search sizes
C
      DF     = RFREQ (2, 1) - RFREQ (1, 1)
      RTAMB  = 1.0 / (DTIME * SECPDY)
      SBDAMB = 1.0 / DF
      NUMT   = NINT ((PTIME(NUMDP) - PTIME(1)) / DTIME) + 1
      NUMF   = NAXIS(CHNIDX)
      NT     = 2**RBLOAT * NXPOW2 (NUMT)
      NF     = 2**DBLOAT * NXPOW2 (NUMF)
      DR     = RTAMB / NT
      DSBD   = SBDAMB / NF
      DO 30 J = 1, NUMIF
         IFFREQ(J) = RFREQ (1, J)
         DO 10 I = 1, NUMF
            IF (RFREQ (I, J).LT.IFFREQ(J)) THEN
               IFFREQ (J) = RFREQ (I, J)
               END IF
 10         CONTINUE
         IF (J.EQ.1) THEN
            LOFREQ = IFFREQ(J)
            HIFREQ = IFFREQ(J)
         ELSE
            DO 20 I = 1, J - 1
               IF (IFFREQ(I).EQ.IFFREQ(J)) THEN
                  MSGTXT = 'DUPLICATE IF DETECTED: CAN NOT USE MK3 MODE'
                  CALL MSGWRT (9)
                  IRET = 1
                  GO TO 999
                  END IF
 20            CONTINUE
            LOFREQ = MIN (LOFREQ, IFFREQ(J))
            HIFREQ = MAX (HIFREQ, IFFREQ(J))
            END IF
 30      CONTINUE
      DIF = IFINC (IFFREQ, NUMIF)
      MBDAMB = 1.0 / DIF
      NIF = 2 ** DBLOAT * NXPOW2 (NINT ((HIFREQ - LOFREQ) / DIF) + 1)
      DMBD = MBDAMB / NIF
C
C     Blank output solutions:
C
      CALL BSBLNK (1)
C
C     Check that there is more than one IF channel so that a multiband
C     delay search is meaningful:
C
      IF (NUMIF.LE.1) THEN
         IRET = 1
         MSGTXT = 'SOLIND: NEED MORE THAN ONE IF FOR DELAY SEARCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
C     Check that there is more than one frequency channel so that a
C     single-band delay search is meaningful:
C
      IF (NUMF.LE.1) THEN
         IRET = 1
         MSGTXT = 'SOLIND: NEED MORE THAN ONE CHANNEL FOR DELAY SEARCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
C     Check that there are enough integrations in the solution interval:
C
      IF (NUMT.LE.1) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         MSGTXT = '    (insufficient data)'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Check FFT limit:
C
      IF (NIF.GT.FFTMAX) THEN
         IRET = 2
         MSGTXT = 'IF FFT EXCEEDS LIMIT FOR THIS SYSTEM'
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NF.GT.FFTMAX) THEN
         IRET = 2
         MSGTXT = 'CHANNEL FFT EXCEEDS LIMIT FOR THIS SYSTEM'
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NT.GT.FFTMAX) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Calculate rate window size in cells:
C
      IF (SRCHWN(2, 3, SRCHIX).GT.0.0) THEN
         MR = NINT (SRCHWN(2, 3, SRCHIX) / DR + 0.5)
         MR = MAX (1, MR)
         MR = MIN (MR, NT)
      ELSE
         MR = NT
         END IF
C
C     Calculate delay windows in cells:
C
      IF (SRCHWN(2, 2, SRCHIX).GT.0.0) THEN
         MSBD = NINT (SRCHWN(2, 2, SRCHIX) / DSBD + 0.5)
         MSBD = MAX (1, MSBD)
         MSBD = MIN (MSBD, NF)
      ELSE
         MSBD = NF
         END IF
      IF (SRCHWN(2, 1, SRCHIX).GT.0.0) THEN
         MMBD = NINT (SRCHWN(2, 1, SRCHIX) / DMBD + 0.5)
         MMBD = MAX (1, MMBD)
         MMBD = MIN (MMBD, NIF)
      ELSE
         MMBD = NIF
         END IF
C
C     Search accelerations:
C
      ACCEL = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
      NUMACC = 0
 70   IF (ACCEL
     *  .LE.(SRCHWN(1, 4, SRCHIX) + SRCHWN(2, 4, SRCHIX) / 2.0)) THEN
         NUMACC = NUMACC + 1
C
C        Accumulate data:
C
         DO 60 K = 1, NIF
            DO 50 J = 1, NF
               DO 40 I = 1, NT
                  DATA(I, J, K) = CMPLX (0.0, 0.0)
 40               CONTINUE
 50            CONTINUE
 60         CONTINUE
         SUMAMP = 0.0
         DO 80 I = 1, NUMDP
            IF (PPOL(I).EQ.POL) THEN
               TIMIND = NINT ((PTIME(I) - PTIME(1)) / DTIME) + 1
               FRQIDX = PCHAN(I)
               IIFIDX  = NINT ((IFFREQ(PIF(I)) - LOFREQ) / DIF) + 1
               PHCOR  = -TWOPI
     *            * (SRCHWN(1, 2, SRCHIX)
     *               * (RFREQ (PCHAN(I), PIF(I)) - UVFREQ)
     *               + SRCHWN(1, 1, SRCHIX) * (IFFREQ(PIF(I)) - LOFREQ)
     *               + SRCHWN(1, 3, SRCHIX) * SECPDY
     *               * (PTIME(I) - CTIME)
     *               + 0.5 * ACCEL * (SECPDY * (PTIME(I) - CTIME))**2)
               DATA(TIMIND, FRQIDX, IFIDX) =  CMPLX (COS (PHCOR),
     *                                               SIN (PHCOR))
     *            * CMPLX (PDATA(1, I), PDATA(2, I))
               SUMAMP = SUMAMP + SQRT (PDATA(1, I)**2 + PDATA(2, I)**2)
               END IF
 80         CONTINUE
C
C        Trap the case where no data was available for the current
C        polarization and IF:
C
         IF (SUMAMP.LE.0.0) THEN
            WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3),
     *         IT(4)
            CALL MSGWRT (6)
            MSGTXT = '    (insufficient data)'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C
         CALL FRSRCI (NT, NF, NIF, MR, MSBD, MMBD, DATA, TIMDIM, CHNDIM,
     *      ACCBUF(4, NUMACC), ACCBUF(5, NUMACC), ACCBUF(6, NUMACC),
     *      ACCBUF(1, NUMACC), ACCBUF(2, NUMACC), ACCBUF(3, NUMACC),
     *      RTGOOD, SBGOOD, MBGOOD, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SOLMK3: FATAL ERROR DETECTED IN FRSRCI'
            CALL MSGWRT (9)
            GO TO 999
            END IF
         IF (SRCHWN(2, 4, SRCHIX).GT.0.0) THEN
            ACCEL = ACCEL + SRCHDA(SRCHIX)
            GO TO 70
            END IF
         END IF
C
C     Find acceleration with best signal:
C
      J = 1
      DO 90 I = 1, NUMACC
         IF (ACCBUF(1, I).GT.ACCBUF(1, J)) THEN
            J = I
            END IF
 90      CONTINUE
C
C     Check good solution criteria
C
      IF ((ACCBUF(1, J) / ACCBUF(3, J)).LT.MINSNR) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1030) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE IF ((ACCBUF(1, J) / SUMAMP).LT.CTHRSH) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1031) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         RMBD      = -ACCBUF(6, J) * DMBD
         MBDERR    = 0.5 * DMBD
         RSBD(1)   = -ACCBUF(5, J) * DSBD + SRCHWN(1, 2, SRCHIX)
         SBDERR(1) = 0.5 * DSBD
         RRATE(1)  = -ACCBUF(4, J) * DR + SRCHWN(1, 3, SRCHIX)
         RTERR(1)  = 0.5 * DR
         RACCEL(1) = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
     *      + SRCHDA(SRCHIX) * (J - 1)
         ACCERR(1) = MAX (0.0, 0.5 * SRCHDA(SRCHIX))
         RPHASE(1) = RAD2DG * ACCBUF(2, J)
     *      - 360.0 * RSBD(1)
     *              * ((RFREQ (NAXIS(CHNIDX), NUMIF) + RFREQ (1 , 1))
     *                 / 2.0 - UVFREQ)
         PHSERR(1) = RAD2DG * ACCBUF(3, J) / ACCBUF(1, J)
         VAMP(1)   = ACCBUF(1, J)
         SAMP(1)   = SUMAMP
C
C        Interpolate acceleration if not at edge of search range:
C
         IF ((1.LT.J) .AND. (J.LT.NUMACC)) THEN
            Y(1) = ACCBUF(1, J-1)
            Y(2) = ACCBUF(1, J)
            Y(3) = ACCBUF(1, J+1)
            CALL SVANDT (2, X, Y)
            ADJUST = -0.5  * Y(2) / Y(3)
            RACCEL(1) = RACCEL(1) + ADJUST * SRCHDA(SRCHIX)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Skipped ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2,':', I2.2)
 1001 FORMAT ('    (more than ', I6, ' FFT points)')
 1030 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low SNR)')
 1031 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':',
     *   I2.2, ':', I2.2, ' (low coherence)')
      END
      SUBROUTINE SOLRAT (POL, DTIME, SRCHIX, DATA, TIMDIM, CHNDIM,
     *   IFDIM, ACCBUF, ACCDIM, NUMDP, PTIME, PDATA, PPOL, IRET)
C-----------------------------------------------------------------------
C   Search for fringes in the given polarization assuming rate errors
C   only
C
C   Inputs:
C      POL       I            Polarization number
C      DTIME     R            Time spacing in days.
C      SRCHIX    I            Index of search parameters
C      CHNDIM    I            Channel dimension of data grid
C      IFDIM     I            IF dimension of data grid
C      ACCDIM    I            Dimension of acceleration buffer
C      NUMDP     I            Number of data points in solution
C                             interval
C
C   In/Out:
C      DATA      C(TIMDIM, CHNDIM, IFDIM)
C                             Data grid
C      ACCBUF    R(6, ACCDIM) Acceleration buffer; first axis
C                             enumerates amplitude, phase, noise,
C                             rate, single-band delay, multiband-delay
C      PTIME     R(MAXDP)     List of timestamps
C      PDATA     R(2, MAXDP)
C                            Phase data, stored as real/imaginary
C                            components scaled by data weight
C      PPOL      I(MAXDP)     List of polarization numbers
C   Output
C      IRET      I            Return code: 0 => OK
C                                          anything else: unrecoverable
C                                                         error
C-----------------------------------------------------------------------
      INTEGER   POL
      REAL      DTIME
      INTEGER   SRCHIX, TIMDIM, CHNDIM, IFDIM, ACCDIM, IRET
      COMPLEX   DATA(TIMDIM, CHNDIM, IFDIM)
      REAL      ACCBUF(6, ACCDIM)
      INTEGER   NUMDP
      REAL      PTIME(*), PDATA(2, *)
      INTEGER   PPOL(*)
C
      REAL      SECPDY
      PARAMETER (SECPDY = 24.0 * 60.0 * 60.0)
C                                       Seconds per day
C
      INTEGER   NUMT
C                                       Number of times in solution
C                                       interval
C
      INTEGER   NT
C                                       Length of delay/rate transform
C
      INTEGER   MR
C                                       Number of rate cell to search
C
      REAL      DR
C                                       Rate cell increment
C
      REAL      ACCEL
C                                       Current acceleration value
C
      INTEGER   NUMACC
C                                       Number of accelerations searched
C
      INTEGER   TIMIND
C                                       Time index
C
      REAL      PHCOR
C                                       Phase correction
C
      REAL      SUMAMP
C                                       Sum of amplitudes
C
      REAL      ADJUST
C                                       Acceleration interpolation
C                                       adjustment
C
      REAL      X(3), Y(3)
C                                       Interpolation arrays
C
      INTEGER   IT(4)
C                                       Time broken down into integer
C                                       components
C
      LOGICAL   RTGOOD, SBGOOD, MBGOOD
      INTEGER   I, J
      CHARACTER CDUMMY
C
      INTEGER   NXPOW2
      EXTERNAL  NXPOW2
C
      INCLUDE 'INCS:PFFT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSREC.INC'
      INCLUDE 'CONTROL.INC'
      INCLUDE 'SEARCH.INC'
      INCLUDE 'UVINFO.INC'
C
      DATA X / -1.0, 0.0, +1.0 /
C-----------------------------------------------------------------------
      IRET = 0
      RTAMB = 1.0 / (DTIME * SECPDY)
      NUMT  = NINT ((PTIME(NUMDP) - PTIME(1)) / DTIME) + 1
      NT    = 2**RBLOAT * NXPOW2 (NUMT)
      DR    = RTAMB / NT
C
C     Blank output solutions:
C
      CALL BSBLNK (1)
C
C     Check that there are enough integrations in the solution interval:
C
      IF (NUMT.LE.1) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         MSGTXT = '    (insufficient data)'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Check FFT limit:
C
      IF (NT.GT.FFTMAX) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3), IT(4)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1001) FFTMAX
         CALL MSGWRT (6)
         GO TO 999
         END IF
C
C     Calculate rate window size in cells:
C
      IF (SRCHWN(2, 3, SRCHIX).GT.0.0) THEN
         MR = NINT (SRCHWN(2, 3, SRCHIX) / DR + 0.5)
         MR = MAX (1, MR)
         MR = MIN (MR, NT)
      ELSE
         MR = NT
         END IF
C
C     Search accelerations:
C
      ACCEL = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
      NUMACC = 0
   10 IF (ACCEL
     *  .LE.(SRCHWN(1, 4, SRCHIX) + SRCHWN(2, 4, SRCHIX) / 2.0)) THEN
         NUMACC = NUMACC + 1
C
C        Clear the data grid:
C
         DO 15 I = 1, NT
            DATA(I, 1, 1) = CMPLX (0.0, 0.0)
 15         CONTINUE
C
C        Accumulate data:
C
         SUMAMP = 0.0
         DO 20 I = 1, NUMDP
            IF (PPOL(I).EQ.POL) THEN
               TIMIND = NINT ((PTIME(I) - PTIME(1)) / DTIME) + 1
               PHCOR  = -TWOPI * (SRCHWN(1, 3, SRCHIX) * SECPDY
     *            * (PTIME(I) - CTIME) + 0.5 * ACCEL * (SECPDY
     *            * (PTIME(I) - CTIME))**2)
               DATA(TIMIND, 1, 1) =  DATA(TIMIND, 1, 1)
     *            + CMPLX (COS (PHCOR), SIN (PHCOR))
     *            * CMPLX (PDATA(1, I), PDATA(2, I))
               SUMAMP = SUMAMP + SQRT (PDATA(1, I)**2 + PDATA(2, I)**2)
               END IF
 20         CONTINUE
C
C        Trap the case where no data was available for the current
C        polarization:
C
         IF (SUMAMP.LE.0.0) THEN
            WRITE (MSGTXT, 1000) POLLAB(POL), IT(1), IT(2), IT(3),
     *         IT(4)
            CALL MSGWRT (6)
            MSGTXT = '    (insufficient data)'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C
         CALL FRSRCI (NT, 1, 1, MR, 1, 1, DATA, TIMDIM, CHNDIM,
     *      ACCBUF(4, NUMACC), ACCBUF(5, NUMACC), ACCBUF(6, NUMACC),
     *      ACCBUF(1, NUMACC), ACCBUF(2, NUMACC), ACCBUF(3, NUMACC),
     *      RTGOOD, SBGOOD, MBGOOD, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SOLRAT: FATAL ERROR DETECTED IN FRSRCI'
            CALL MSGWRT (9)
            GO TO 999
            END IF
         IF (SRCHWN(2, 4, SRCHIX).GT.0.0) THEN
            ACCEL = ACCEL + SRCHDA(SRCHIX)
            GO TO 10
            END IF
         END IF
C
C     Find acceleration with best signal:
C
      J = 1
      DO 30 I = 1, NUMACC
         IF (ACCBUF(1, I).GT.ACCBUF(1, J)) THEN
            J = I
            END IF
 30      CONTINUE
C
C     Check good solution criteria
C
      IF ((ACCBUF(1, J) / ACCBUF(3, J)).LT.MINSNR) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1030) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE IF ((ACCBUF(1, J) / SUMAMP).LT.CTHRSH) THEN
         CALL T2DHMS (REAL(CTIME), CDUMMY, IT)
         WRITE (MSGTXT, 1031) POLLAB(POL), IT(1), IT(2), IT(3),
     *      IT(4)
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         RSBD(1)  = 0.0
         SBDERR(1) = 1.0E-9
         RRATE(1) = -ACCBUF(4, J) * DR + SRCHWN(1, 3, SRCHIX)
         RTERR(1) = 0.5 * DR
         RACCEL(1) = SRCHWN(1, 4, SRCHIX) - SRCHWN(2, 4, SRCHIX) / 2.0
     *      + SRCHDA(SRCHIX) * (J - 1)
         ACCERR(1) = MAX (0.0, 0.5 * SRCHDA(SRCHIX))
         RPHASE(1) = RAD2DG * ACCBUF(2, J)
         PHSERR(1) = RAD2DG * ACCBUF(3, J) / ACCBUF(1, J)
         VAMP(1)   = ACCBUF(1, J)
         SAMP(1)   = SUMAMP
C
C        Interpolate acceleration if not at edge of search range:
C
         IF ((1.LT.J) .AND. (J.LT.NUMACC)) THEN
            Y(1) = ACCBUF(1, J-1)
            Y(2) = ACCBUF(1, J)
            Y(3) = ACCBUF(1, J+1)
            CALL SVANDT (2, X, Y)
            ADJUST = -0.5 * Y(2) / Y(3)
            RACCEL(1) = RACCEL(1) + ADJUST * SRCHDA(SRCHIX)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Skipped ', A2, ' at ', I3, '/', I2.2, ':', I2.2, ':',
     *   I2.2)
 1001 FORMAT ('    (more than ', I6, ' FFT points)')
 1030 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':', I2.2, ':',
     *   I2.2, ' (low SNR)')
 1031 FORMAT ('Rejected ', A2, ' at ', I3, '/', I2.2, ':', I2.2, ':',
     *   I2.2, ' (low coherence)')
      END
      SUBROUTINE BLHIST (ADVRBS, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Update the history file.
C
C   Inputs:
C      ADVRBS   C*(*)     The name of the INPUTS object containing the
C                         values of the input adverbs.
C      UVFILE   C*(*)     The name of the UVDATA object associated with
C                         the data file.
C
C   Output:
C      IRET     I         Return status: 0 => history file updated
C                                        anything else => failed
C-----------------------------------------------------------------------
      CHARACTER ADVRBS*(*), UVFILE*(*)
      INTEGER   IRET
C
      INCLUDE 'INPUTS.INC'
C------------------------------------------------------------------------
      CALL OHTIME (UVFILE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OHLIST (ADVRBS, AVNAME, NPARMS, UVFILE, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
      END
      INTEGER FUNCTION NXPOW2 (I)
C-----------------------------------------------------------------------
C   The smallest power of 2 that is greater than or equal to I.
C
C   Input
C      I           I            Minimum value
C-----------------------------------------------------------------------
      INTEGER   I
C
      INTEGER   POW
C-----------------------------------------------------------------------
      POW = INT (LOG (REAL (I)) / LOG (2.0))
      NXPOW2 = 2 ** POW
C                                       I / 2 < NXPOW2 <= I
      IF (NXPOW2.LT.I) THEN
         NXPOW2 = 2 * NXPOW2
         END IF
      END
      DOUBLE PRECISION FUNCTION RFREQ (CHAN, IIF)
C-----------------------------------------------------------------------
C   The reference frequency for channel CHAN and IF IIF.
C
C   Inputs
C      CHAN    I      Channel number
C      IIF     I      IF number
C-----------------------------------------------------------------------
      INTEGER   CHAN, IIF
C
      INCLUDE 'UVINFO.INC'
C-----------------------------------------------------------------------
      IF ((IFIDX.EQ.-1) .OR. (CHNIDX.LT.IFIDX)) THEN
         RFREQ = FREQS (NAXIS(CHNIDX) * (IIF - 1) + CHAN)
      ELSE
         RFREQ = FREQS (NAXIS(IFIDX) * (CHAN - 1) + IIF)
         END IF
      END
      DOUBLE PRECISION FUNCTION IFINC (IFFREQ, NIF)
C-----------------------------------------------------------------------
C   The grid spacing for IF frequencies.
C
C   Inputs:
C      IFFREQ   D(*)     IF reference frequencies
C      NIF      I        Number of IFs
C-----------------------------------------------------------------------
      DOUBLE PRECISION IFFREQ(*)
      INTEGER   NIF
C
C   TOL      D        Tolerance used in testing for termination (Hz)
C
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 10.0)
      INTEGER   IMAX, I
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION D(MAXIF), DIFF, MAX, MIN
C
      INTEGER   IDAMAX
      DOUBLE PRECISION DMIN
      EXTERNAL  IDAMAX, DMIN
C-----------------------------------------------------------------------
C
C     Compute the frequency differences and use a generalized form of
C     Euclid's algorithm for finding their gcd.
C
      DO 10 I = 1, NIF - 1
         D(I) = ABS(IFFREQ(I+1) - IFFREQ(I))
 10      CONTINUE
C
C
C        Find the maximum and minimum differences
C
 20   CONTINUE
         IMAX = IDAMAX (D, NIF - 1)
         MAX = D(IMAX)
         MIN = DMIN (D, NIF - 1)
C                                        Find range of differences:
         DIFF = MAX - MIN
C                                        Test for termination:
         IF (DIFF.GT.TOL) THEN
C                                        Reduce maximum by minimum
            D(IMAX) = DIFF
            GO TO 20
            END IF
C                                        The GCD is what we want:
      IFINC = D(1)
C
      END
      INTEGER FUNCTION IDAMAX (S, N)
C-----------------------------------------------------------------------
C The index of the first minimum in a single-precision vector
C S of length N.  Compatible with the BLAS1 routine.
C
C Inputs:
C    S     D(*)       The vector
C    N     I          The length of the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION S(*)
      INTEGER   N
C
      INTEGER   I
      DOUBLE PRECISION MAX
C-----------------------------------------------------------------------
      IDAMAX = 1
      MAX = S(1)
      DO 10 I = 2, N
         IF (S(I).GT.MAX) THEN
            MAX = S(I)
            IDAMAX = I
            END IF
 10      CONTINUE
C
      END
      DOUBLE PRECISION FUNCTION DMIN (D, N)
C-----------------------------------------------------------------------
C Return the minimum value in a double precision vector of length N
C
C Inputs:
C    D       D(*)       The vector
C    N       I          The number of elements in the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION D(*)
      INTEGER   N
C
      INTEGER   I
C-----------------------------------------------------------------------
      DMIN = D(1)
      DO 10 I = 2, N
         IF (D(I).LT.DMIN) DMIN = D(I)
 10      CONTINUE
      END
      SUBROUTINE BLREPT (CTIME, POL, NUMIF, RMBD, RSBD, RRATE, RACCEL)
C-----------------------------------------------------------------------
C   Report the fringe-search results.  Print a header giving the centre
C   time and the polarization followed by a line for each IF giving the
C   multiband (first IF only) and single-band delays in nanoseconds,
C   the rate in mHz and the acceleration in uHz/sec.
C
C   Inputs:
C      CTIME    D          Centre time in days (experiment time)
C      POL      C*2        Polarization string
C      NUMIF    I          Number of IFs for which there are solutions
C      RMBD     R          Residual multiband delay (sec)
C      RSBD     R(*)       Residual single-band delay for each IF (sec)
C      RRATE    R(*)       Residual rate for each IF (Hz)
C      RACCEL   R(*)       Residual acceleration (Hz/sec)
C
C   NUMIF should be greater than 0 and is assumed to be less than 1000.
C-----------------------------------------------------------------------
      DOUBLE PRECISION CTIME
      CHARACTER        POL*2
      INTEGER          NUMIF
      REAL             RMBD, RSBD(*), RRATE(*), RACCEL(*)
C
C   Local variables
C
C      D               Day
C      H               Hour
C      M               Minute
C      S               Second
C
C      LINE            Line number
C      BUFFER          Formatting buffer
C      INDEF           String to use for blanked numbers
C
      INTEGER   D, H, M, LINE
      REAL      S
      CHARACTER BUFFER*9, INDEF*9
      PARAMETER (INDEF = ' blank   ')
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Break down the time:
C
      D = INT (CTIME)
      S = 24.0 * (CTIME - D)
      H = INT (S)
      S = 60.0 * (S - H)
      M = INT (S)
      S = 60.0 * (S - M)
C
C     Print solution header:
C
      WRITE (MSGTXT, 1000) D, H, M, S, POL
      CALL MSGWRT (4)
C
C     Print solutions for each IF
C
      DO 10 LINE = 1, NUMIF
         WRITE (MSGTXT, 1001) LINE
         IF (LINE.EQ.1) THEN
            IF (RMBD.NE.FBLANK) THEN
               WRITE (BUFFER, 1002) 1.0E9 * RMBD
            ELSE
               BUFFER = INDEF
               END IF
            MSGTXT(10:18) = BUFFER
         ELSE
            MSGTXT(5:18) = '            '
            END IF
         IF (RSBD(LINE).NE.FBLANK) THEN
            WRITE (BUFFER, 1002) 1.0E9 * RSBD(LINE)
         ELSE
            BUFFER = INDEF
            END IF
         MSGTXT(26:34) = BUFFER
         IF (RRATE(LINE).NE.FBLANK) THEN
            WRITE (BUFFER, 1002) 1.0E3 * RRATE(LINE)
         ELSE
            BUFFER = INDEF
            END IF
         MSGTXT(42:50) = BUFFER

         IF (RACCEL(LINE).NE.FBLANK) THEN
            WRITE (BUFFER, 1002) 1.0E6 * RACCEL(LINE)
         ELSE
            BUFFER = INDEF
            END IF
         MSGTXT(56:64) = BUFFER
         CALL MSGWRT(4)
 10      CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3.3, '/', I2, ':', I2, ':', F4.1, 2X, A2)
 1001 FORMAT (I3, ' mbd = xxxx.xxx  sbd = xxxx.xxx   rt = xxxx.xxx',
     *     '  a = xxxx.xxx')
 1002 FORMAT (F9.3)
      END
      SUBROUTINE DATDIM (DDIM)
C-----------------------------------------------------------------------
C   Fill DDIM with the dimensions of the largest search grid.  DDIM(1)
C   gives the length of the time/rate axis, DDIM(2) the length of the
C   channel/single-band delay axis, and DDIM(3) the length of the IF/
C   multiband delay axis.
C
C   Output:
C      DDIM     I(3)     Minimum dimensions of data grid
C
C   Preconditions:
C      1 <= MODE <= 4
C      RDCTRL has been called
C      INIUVI has been called
C      MODE == 2 implies NAXIS(CHNIDX) > 1
C
C   Postconditions:
C      for all i such that 1 <= i <= 3, DDIM(i) >= 1
C-----------------------------------------------------------------------
      INTEGER   DDIM(3)
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   NT
C                                       Maximum number of integrations
C                                       in a solution interval
C
      INTEGER   NF
C                                       Number of frequency channels
C
      INTEGER   NIF
C                                       Number of IF grid cells
C
      INTEGER   IFCELL
C                                       Number of cells on IF axis
      DOUBLE PRECISION MAXFRQ
      DOUBLE PRECISION MINFRQ
C                                       Maximum and minimum frequencies
C
      DOUBLE PRECISION DF
C                                       Frequency increment
C
      DOUBLE PRECISION IFFREQ(MAXIF)
C                                       If reference frequencies
C
      INTEGER   I, J
C
      REAL      MXSOLI
      EXTERNAL  MXSOLI
      INTEGER   NXPOW2
      EXTERNAL  NXPOW2
      DOUBLE PRECISION IFINC
      EXTERNAL  IFINC
      DOUBLE PRECISION RFREQ
      EXTERNAL  RFREQ
C
      INCLUDE 'CONTROL.INC'
      INCLUDE 'UVINFO.INC'
C-----------------------------------------------------------------------
C
C     Fix the size of the time axis:
C
      NT = NINT (MXSOLI() / MININT) + 1
      DDIM(1) = 2**RBLOAT * NXPOW2 (NT)
C
C     Fix the size of the channel axis:
C
      IF ((MODE.EQ.1) .OR. (MODE.EQ.3)) THEN
         DDIM(2) = 2**DBLOAT * NXPOW2 (NAXIS(CHNIDX))
      ELSE IF (MODE.EQ.2) THEN
         MAXFRQ = RFREQ(1, 1)
         MINFRQ = RFREQ(1, 1)
         IF (IFIDX.LT.1) THEN
            IFCELL = 1
         ELSE
            IFCELL = NAXIS(IFIDX)
            END IF
C
C        Inv: MAXFRQ = max (RFREQ(:, 1:J))
C             and MINFRQ = min (RFREQ(:, 1:J))
C        Note: Potentially parallel reduction
C
         DO 20 J = 1, IFCELL
C
C           MAXFRQ = max (RFREQ(:, 1:J-1))
C           and MINFRQ = min (RFREQ(:, 1:J-1))
C
C           Inv: MAXFRQ = max (max (RFREQ(:, 1:J-1)),
C                              max (RFREQ(1:I, J)))
C                and MINFRQ = min (min (RFREQ(:, 1:J-1)),
C                                  min (RFREQ(1:I, J)))
C
            DO 10 I = 1, NAXIS(CHNIDX)
C
C              MAXFRQ = max (max (RFREQ(:, 1:J-1)),
C                            max (RFREQ(1:I-1, J)))
C              and MINFRQ = min (min (RFREQ(:, 1:J-1)),
C                                min (RFREQ(1:I-1, J)))
C
               MAXFRQ = MAX (MAXFRQ, RFREQ (I, J))
               MINFRQ = MIN (MINFRQ, RFREQ (I, J))
 10            CONTINUE
 20         CONTINUE
         DF = RFREQ (2, 1) - RFREQ (1, 1)
         NF = NINT ((MAXFRQ - MINFRQ) / DF) + 1
         DDIM(2) = 2**DBLOAT * NXPOW2 (NF)
      ELSE IF (MODE.EQ.4) THEN
         DDIM(2) = 1
         END IF
C
C     Establish size of IF axis:
C
      IF (MODE.EQ.3) THEN
         IF (IFIDX.LT.1) THEN
            IFCELL = 1
         ELSE
            IFCELL = NAXIS(IFIDX)
            END IF
C
C        Inv: for all k, 1 <= k <= J, IFFREQ(k) = min (RFREQ(:, k))
C             and MAXFRQ = max (IFFREQ(1:J))
C             and MINFRQ = min (IFFREQ(1:J))
C
         DO 40 J = 1, IFCELL
            IFFREQ(J) = RFREQ (1, J)
C
C           Inv: IFFREQ(J) = min (RFREQ (1:I, J))
C
            DO 30 I = 1, NAXIS(CHNIDX)
               IFFREQ(J) = MIN (IFFREQ(J), RFREQ (I, J))
 30            CONTINUE
            IF (J.EQ.1) THEN
               MAXFRQ = IFFREQ(J)
               MINFRQ = IFFREQ(J)
            ELSE
               MAXFRQ = MAX (MAXFRQ, IFFREQ(J))
               MINFRQ = MIN (MINFRQ, IFFREQ(J))
               END IF
 40         CONTINUE
         NIF = NINT ((MAXFRQ - MINFRQ) / IFINC (IFFREQ, IFCELL)) + 1
         DDIM(3) = 2**DBLOAT * NXPOW2 (NIF)
      ELSE
         DDIM(3) = 1
         END IF
C
      END
      REAL FUNCTION MXSOLI ()
C-----------------------------------------------------------------------
C   The maximum solution interval to be used.
C
C   Preconditions:
C      INISRH has been called
C-----------------------------------------------------------------------
      INTEGER   I
C
      INCLUDE 'SEARCH.INC'
C-----------------------------------------------------------------------
      MXSOLI = 0.0
C
C     Inv: MXSOLI = max (SRCHSI(1:I))
C
      DO 10 I = 1, NMSRCH
         MXSOLI = MAX (MXSOLI, SRCHSI(I))
 10      CONTINUE
C
      END
      INTEGER FUNCTION MXASTP ()
C-----------------------------------------------------------------------
C   Maximum number of acceleration steps.
C
C   Preconditions:
C      INISRH has been called
C-----------------------------------------------------------------------
      INTEGER   I
      INTEGER   NUMSTP
C
      INCLUDE 'SEARCH.INC'
C-----------------------------------------------------------------------
      MXASTP = 1
      DO 10 I = 1, NMSRCH
         IF ((SRCHDA(I).GT.0.0) .AND. (SRCHWN(2, 4, I).GT.0.0)) THEN
            NUMSTP = NINT (SRCHWN(2, 4, I) / SRCHDA(I)) + 1
            MXASTP = MAX (MXASTP, NUMSTP)
            END IF
 10      CONTINUE
C
      END
      SUBROUTINE MEMFAI
C-----------------------------------------------------------------------
C   Report that memory allocation has failed and suggest remedies
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'FAILED TO ALLOCATE DATA AREAS'
      CALL MSGWRT (9)
      MSGTXT = 'One of the following strategies may help'
      CALL MSGWRT (4)
      MSGTXT = '    (1) Reduce the longest solution interval'
      CALL MSGWRT (4)
      MSGTXT = '    (2) Increase the shortest integration time '
      CALL MSGWRT (4)
      MSGTXT = '        (APARM(1)).'
      CALL MSGWRT (4)
      MSGTXT = '    (3) Average over time or frequency.'
      MSGTXT = 'If none of these are appropriate then try running'
      CALL MSGWRT (4)
      MSGTXT = 'on a machine with a larger virtual memory.'
      CALL MSGWRT (4)
C
      END
      SUBROUTINE BSBLNK (I)
C-----------------------------------------------------------------------
C   Blank entries for IF I in the current BS table record.
C
C   Input:
C      I      I          IF number
C-----------------------------------------------------------------------
      INTEGER   I
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'BSREC.INC'
C-----------------------------------------------------------------------
      RSBD(I)   = FBLANK
      SBDERR(I) = FBLANK
      RRATE(I)  = FBLANK
      RTERR(I)  = FBLANK
      RACCEL(I) = FBLANK
      ACCERR(I) = FBLANK
      RPHASE(I) = FBLANK
      PHSERR(I) = FBLANK
      VAMP(I)   = FBLANK
      SAMP(I)   = FBLANK
C
      END
      SUBROUTINE STWARN (TI, MINTI)
C-----------------------------------------------------------------------
C   Warn the user about integration times shorter than the minimum
C   used to calculate array sizes.
C
C   Inputs:
C      TI       R       Time interval found
C      MINTI    R       Minimum time interval accepted
C-----------------------------------------------------------------------
      REAL      TI, MINTI
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      WRITE (MSGTXT, 1000) 24.0 * 3600.0 * TI
      CALL MSGWRT (6)
      WRITE (MSGTXT, 1001) 24.0 * 3600.0 * MINTI
      CALL MSGWRT (6)
      WRITE (MSGTXT, 1002)
      CALL MSGWRT (6)
      WRITE (MSGTXT, 1003)
      CALL MSGWRT (6)
      WRITE (MSGTXT, 1004)
      CALL MSGWRT (6)
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: found data points separated by ', F9.6,
     *        ' seconds.')
 1001 FORMAT ('Minimum separation was declared to be ', F9.6,
     *        ' seconds.')
 1002 FORMAT ('Shorter separations will be ignored but this may '
     *        , 'degrade the')
 1003 FORMAT ('quality of the fringe solutions.  Abort BLING and '
     *        , 'reset APARM(1)')
 1004 FORMAT ('if this is not acceptable.')
      END
      SUBROUTINE FRSRCI (NT, NF, NIF, NR, NSBD, NMBD, DATA, LD1, LD2,
     *   RT, SBD, MBD, PEAK, PHASE, NOISE, RTGOOD, SBGOOD, MBGOOD, IRET)
C-----------------------------------------------------------------------
C   Perform an FFT search for fringes in multiband and single-band
C   delay and rate and use second-order polynomial interpolation to
C   refine the fringe position if the fringe is not at the edge
C   of the search window.
C
C   Inputs:
C      NT       I            Number of times to transform
C      NF       I            Number of frequencies to transform
C      NIF      I            Number of gridded IFs to transform
C      NR       I            Number of rate cells to search
C      NSBD     I            Number of single-band delays to search
C      NMBD     I            Number of multiband delays to search
C      LD1      I            Leading dimension of DATA array
C      LD2      I            Second dimension of DATA array
C
C   In/out:
C      DATA     C(LD1, LD2, *)  Data array (dimensions are time,
C                               channel and IF)
C
C   Outputs:
C      RT       R            Residual rate in cells
C      SBD      R            Residual single-band delay in cells
C      MBD      R            Residual multiband delay in cells
C      PEAK     R            Peak amplitude in transform
C      PHASE    R            Phase in radians at peak
C      NOISE    R            Noise level in transform
C      RTGOOD   L            Was rate interpolated
C      SBGOOD   L            Was single-band delay interpolated
C      MBGOOD   L            Was multiband delay interpolated
C      IRET     I            Return status: 0 - normal
C                                           non-zero - memory system
C                                                      error (fatal)
C
C   Preconditions:
C
C      1 <= NT <= LD1
C      1 <= NF <= LD2
C      1 <= NIF <= 3rd dimension of DATA
C      NR <= NT
C      NSBD <= NF
C      NMBD <= NIF
C
C   Postconditions
C
C      PEAK >= 0.0
C      NOISE >= 0.0
C
C-----------------------------------------------------------------------
      INTEGER   NT, NF, NIF, NR, NSBD, NMBD, LD1, LD2, IRET
      COMPLEX   DATA(LD1, LD2, *)
      REAL      RT, SBD, MBD, PEAK, PHASE, NOISE
      LOGICAL   RTGOOD, SBGOOD, MBGOOD
C
      REAL      WSAVE(1)
      SAVE      WSAVE
C                                       Base of dynamically allocated
C                                       FFT work array
C
      LONGINT   WSOFF
      SAVE      WSOFF
C                                       Offset of dynamically allocated
C                                       FFT work array
C
      INTEGER   WSSIZE
      SAVE      WSSIZE
C                                       Size of dynamically allocated
C                                       FFT work array
C
      REAL      SUMAMP
C                                       Sum of transformed amplitudes
C
      REAL      X(3), Y(3), PH(3)
      SAVE      X
C                                       Interpolation buffers
C
      REAL      ADJUST
C                                       Position adjustment from
C                                       interpolation
C
      REAL      PGRAD
C                                       Phase gradient
      INTEGER   I, J, K, P, Q, R
C
      INCLUDE 'INCS:PFFT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA WSSIZE / 0 /
      DATA X / -1.0, 0.0, +1.0 /
C-----------------------------------------------------------------------
      IRET = 0
C
C     Expand WSAVE allocation, if necessary.  If WSSIZE > 0 then space
C     has already be allocated for WSAVE and must be deallocated before
C     allocating the larger area.
C
      IF (WSSIZE.LT.FTWMUL * (NT + NF + NIF) + 3 * FTWADD) THEN
         IF (WSSIZE.GT.0) THEN
            CALL ZMEMRY ('FREE', 'FRSRCI', WSSIZE, WSAVE, WSOFF, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'MEMORY MANAGEMENT FAILED IN FRSRCI'
               CALL MSGWRT (9)
               GO TO 999
               END IF
            END IF
         WSSIZE = (FTWMUL * (NT + NF + NIF) + 3 * FTWADD - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'FRSRCI', WSSIZE, WSAVE, WSOFF, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'MEMORY MANAGEMENT FAILED IN FRSRCI'
            CALL MSGWRT (9)
            GO TO 999
            END IF
         END IF
C
C     Transform the data grid:
C
      CALL QCFT3I (NT, NF, NIF, WSAVE(WSOFF+1))
      CALL QCFT3D (-1, NT, NF, NIF, DATA, LD1, LD2, WSAVE(WSOFF+1))
C
C     Sum the transformed amplitudes and search for the peak in the
C     window.  Note that the window corresponds to the corners of
C     the data "cube".  This could be written in a more optimum
C     fashion but it is unlikely to be worth doing so since the
C     time taken to search the grid will be small compared to the
C     time taken to transform it.
C
C     Note also that NR - NR/2 is NR/2 if NR is even and NR/2 + 1
C     if NR is odd.
C
      SUMAMP = 0.0
      PEAK = 0.0
      DO 30 K = 1, NIF
         DO 20 J = 1, NF
            DO 10 I = 1, NT
               SUMAMP = SUMAMP + ABS (DATA (I, J, K))
               IF (((1.LE.I) .AND. (I.LE.NR - NR/2))
     *             .AND. ((1.LE.J) .AND. (J.LE.NSBD - NSBD/2))
     *             .AND. ((1.LE.K) .AND. (K.LE.NMBD - NMBD/2))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - 1
                  SBD = J - 1
                  MBD = K - 1
               ELSE IF ((((NT - NR/2 + 1).LE.I) .AND. (I.LE.NT))
     *             .AND. ((1.LE.J) .AND. (J.LE.NSBD - NSBD/2))
     *             .AND. ((1.LE.K) .AND. (K.LE.NMBD - NMBD/2))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - NT - 1
                  SBD = J - 1
                  MBD = K - 1
               ELSE IF (((1.LE.I) .AND. (I.LE.(NR - NR/2)))
     *             .AND. (((NF - NSBD/2 + 1).LE.J) .AND. (J.LE.NF))
     *             .AND. ((1.LE.K) .AND. (K.LE.NMBD - NMBD/2))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - 1
                  SBD = J - NF - 1
                  MBD = K - 1
               ELSE IF ((((NT - NR/2 + 1).LE.I) .AND. (I.LE.NT))
     *             .AND. (((NF - NSBD/2 + 1).LE.J) .AND. (J.LE.NF))
     *             .AND. ((1.LE.K) .AND. (K.LE.NMBD - NMBD/2))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - NT - 1
                  SBD = J - NF - 1
                  MBD = K - 1
               ELSE IF (((1.LE.I) .AND. (I.LE.NR - NR/2))
     *             .AND. ((1.LE.J) .AND. (J.LE.NSBD - NSBD/2))
     *             .AND. (((NIF - NMBD/2).LE.K) .AND. (K.LE.NIF))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - 1
                  SBD = J - 1
                  MBD = K - NIF - 1
               ELSE IF ((((NT - NR/2 + 1).LE.I) .AND. (I.LE.NT))
     *             .AND. ((1.LE.J) .AND. (J.LE.NSBD - NSBD/2))
     *             .AND. (((NIF - NMBD/2).LE.K) .AND. (K.LE.NIF))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - NT - 1
                  SBD = J - 1
                  MBD = K - NIF - 1
               ELSE IF (((1.LE.I) .AND. (I.LE.(NR - NR/2)))
     *             .AND. (((NF - NSBD/2 + 1).LE.J) .AND. (J.LE.NF))
     *             .AND. (((NIF - NMBD/2).LE.K) .AND. (K.LE.NIF))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - 1
                  SBD = J - NF - 1
                  MBD = K - NIF - 1
               ELSE IF ((((NT - NR/2 + 1).LE.I) .AND. (I.LE.NT))
     *             .AND. (((NF - NSBD/2 + 1).LE.J) .AND. (J.LE.NF))
     *             .AND. (((NIF - NMBD/2).LE.K) .AND. (K.LE.NIF))
     *             .AND. (ABS (DATA(I, J, K)).GT.PEAK)) THEN
                  PEAK = ABS (DATA(I, J, K))
                  PHASE = ATAN2 (AIMAG (DATA(I, J, K)),
     *                           REAL (DATA(I, J, K)))
                  P = I
                  Q = J
                  R = K
                  RT = I - NT - 1
                  SBD = J - NF - 1
                  MBD = K - NIF - 1
                  END IF
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
C     P, Q, R are now the indices of the peak amplitude within the
C     window while RT, SBD and MBD are the actual rate and delays to
C     the nearest cell.
C
C     The noise level is approximated assuming that everything but the
C     peak in the transformed data array is noise.
C
      NOISE = (SUMAMP - PEAK) / REAL (NT * NF * NIF - 1)
C
C     Refine the rate position by fitting a 2nd degree polynomial to
C     the peak and its neighbouring points if there is a neighbouring
C     point on either side on the rate axis.
C
      RTGOOD = .FALSE.
      IF (NT.GE.3) THEN
         DO 40 I = 1, 3
            IF (P + I - 2.LT.1) THEN
               Y(I) = ABS (DATA(P+I-2 + NT, Q, R))
               PH(I) = ATAN2 (REAL (DATA(P+I-2 + NT, Q, R)),
     *                        AIMAG (DATA(P+I-2 + NT, Q, R)))
            ELSE IF (P + I - 2.GT.NT) THEN
               Y(I) = ABS (DATA(P+I-2 - NT, Q, R))
               PH(I) = ATAN2 (REAL (DATA(P+I-2 - NT, Q, R)),
     *                        AIMAG (DATA(P+I-2 - NT, Q, R)))
            ELSE
               Y(I) = ABS (DATA(P+I-2, Q, R))
               PH(I) = ATAN2 (REAL (DATA(P+I-2, Q, R)),
     *                        AIMAG (DATA(P+I-2, Q, R)))
               END IF
 40         CONTINUE
         CALL SVANDT(2, X, Y)
C
C        Note that peak is not a local maximum unless the coefficient
C        of the second order term is negative (ie. second derivative
C        is negative).  If this is negative we can refine the peak
C        position using the condition that the first derivative
C        (Y(2) + 2 * Y(3) * X) is zero at the peak.
C
         IF (Y(3).LT.0.0) THEN
            RTGOOD = .TRUE.
            ADJUST = -0.5 * Y(2) / Y(3)
            RT = RT +ADJUST
C
C           Interpolate phase assuming less than 1 turn per cell:
C
            IF (ADJUST.GE.0.0) THEN
               PGRAD = PH(3) - PH(2)
            ELSE
               PGRAD = PH(2) - PH(1)
               END IF
            IF (PGRAD.GT.+PI) THEN
               PGRAD = PGRAD - TWOPI
            ELSE IF (PGRAD.LT.-PI) THEN
               PGRAD = PGRAD + TWOPI
               END IF
            PHASE = PHASE + PGRAD * ADJUST
            END IF
         END IF
C
C     Refine the single-band delay position by fitting a 2nd degree
C     polynomial to the peak and its neighbouring points if there
C     is a neighbouring point on either side on the sbd axis.
C
      SBGOOD = .FALSE.
      IF (NF.GE.3) THEN
         DO 50 I = 1, 3
            IF (Q + I - 2.LT.1) THEN
               Y(I) = ABS (DATA(P, Q+I-2 + NF, R))
               PH(I) = ATAN2 (REAL (DATA(P, Q+I-2 + NF, R)),
     *                        AIMAG (DATA(P, Q+I-2 + NF, R)))
            ELSE IF (Q + I - 2.GT.NF) THEN
               Y(I) = ABS (DATA(P, Q+I-2 - NF, R))
               PH(I) = ATAN2 (REAL (DATA(P, Q+I-2 - NF, R)),
     *                        AIMAG (DATA(P, Q+I-2 - NF, R)))
            ELSE
               Y(I) = ABS (DATA(P, Q+I-2, R))
               PH(I) = ATAN2 (REAL (DATA(P, Q+I-2, R)),
     *                        AIMAG (DATA(P, Q+I-2, R)))
               END IF
 50         CONTINUE
         CALL SVANDT(2, X, Y)
C
C        Note that peak is not a local maximum unless the coefficient
C        of the second order term is negative (ie. second derivative
C        is negative).  If this is negative we can refine the peak
C        position using the condition that the first derivative
C        (Y(2) + 2 * Y(3) * X) is zero at the peak.
C
         IF (Y(3).LT.0.0) THEN
            SBGOOD = .TRUE.
            ADJUST = -0.5 * Y(2) / Y(3)
            SBD = SBD +ADJUST
C
C           Interpolate phase assuming less than 1 turn per cell:
C
            IF (ADJUST.GE.0.0) THEN
               PGRAD = PH(3) - PH(2)
            ELSE
               PGRAD = PH(2) - PH(1)
               END IF
            IF (PGRAD.GT.+PI) THEN
               PGRAD = PGRAD - TWOPI
            ELSE IF (PGRAD.LT.-PI) THEN
               PGRAD = PGRAD + TWOPI
               END IF
            PHASE = PHASE + PGRAD * ADJUST
            END IF
         END IF
C
C     Refine the multiband delay position by fitting a 2nd degree
C     polynomial to the peak and its neighbouring points if there
C     is a neighbouring point on either side on the sbd axis.
C
      MBGOOD = .FALSE.
      IF (NIF.GE.3) THEN
         DO 60 I = 1, 3
            IF (R + I - 2.LT.1) THEN
               Y(I) = ABS (DATA(P, Q, R+I-2 + NIF))
               PH(I) = ATAN2 (REAL (DATA(P, Q, R+I-2 + NIF)),
     *                        AIMAG (DATA(P, Q, R+I-2 + NIF)))
            ELSE IF (R + I - 2.GT.NIF) THEN
               Y(I) = ABS (DATA(P, Q, R+I-2 - NIF))
               PH(I) = ATAN2 (REAL (DATA(P, Q, R+I-2 - NIF)),
     *                        AIMAG (DATA(P, Q, R+I-2 - NIF)))
            ELSE
               Y(I) = ABS (DATA(P, Q, R+I-2))
               PH(I) = ATAN2 (REAL (DATA(P, Q, R+I-2)),
     *                        AIMAG (DATA(P, Q, R+I-2)))
               END IF
 60         CONTINUE
         CALL SVANDT (2, X, Y)
C
C        Note that peak is not a local maximum unless the coefficient
C        of the second order term is negative (ie. second derivative
C        is negative).  If this is negative we can refine the peak
C        position using the condition that the first derivative
C        (Y(2) + 2 * Y(3) * X) is zero at the peak.
C
         IF (Y(3).LT.0.0) THEN
            MBGOOD = .TRUE.
            ADJUST = -0.5 * Y(2) / Y(3)
            MBD = MBD +ADJUST
C
C           Interpolate phase assuming less than 1 turn per cell:
C
            IF (ADJUST.GE.0.0) THEN
               PGRAD = PH(3) - PH(2)
            ELSE
               PGRAD = PH(2) - PH(1)
               END IF
            IF (PGRAD.GT.+PI) THEN
               PGRAD = PGRAD - TWOPI
            ELSE IF (PGRAD.LT.-PI) THEN
               PGRAD = PGRAD + TWOPI
               END IF
            PHASE = PHASE + PGRAD * ADJUST
            END IF
         END IF
C
  999 RETURN
C
      END
