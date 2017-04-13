LOCAL INCLUDE 'INPUT.INC'
C                                        Declarations for inputs.
      CHARACTER PROG*6
      PARAMETER (PROG = 'BLAPP ')
      INTEGER   NPARMS
      PARAMETER (NPARMS = 19)
      INTEGER   AVTYPE(NPARMS), AVDIM(2, NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPDATA.INC'
C                                        DATA statements defining
C                                        input parameters.
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'INVERS  ', 'SOURCES ', 'STOKES  ', 'SELBAND ',
     *             'SELFREQ ', 'FREQID  ', 'TIMERANG', 'ANTENNAS',
     *             'SUBARRAY', 'REFANT  ', 'ANTWT',    'OPCODE',
     *             'GAINVER',  'GAINUSE ', 'BADDISK '/
      DATA AVTYPE /OOACAR,     OOACAR,     OOAINT,     OOAINT,
     *             OOAINT,     OOACAR,     OOACAR,     OOARE,
     *             OOARE,      OOAINT,     OOARE,      OOAINT,
     *             OOAINT,     OOAINT,     OOARE,      OOACAR,
     *             OOAINT,     OOAINT,     OOAINT/
      DATA AVDIM  /12, 1,       6, 1,       1, 1,       1, 1,
     *              1, 1,      16,30,       4, 1,       1, 1,
     *              1, 1,       1, 1,       8, 1,      50, 0,
     *              1, 1,       1, 1,      30, 1,       4, 1,
     *              1, 1,       1, 1,      10, 1/
LOCAL END
LOCAL INCLUDE 'BSRECD.INC'
C                                        BS table record.
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION TIME
      REAL      INTERV, VAMP, SAMP, RMBD, MBDERR, MBDAMB, RSBD(MAXIF),
     *   SBDERR(MAXIF), SBDAMB, RRATE(MAXIF), RTERR(MAXIF), RTAMB,
     *   RACCL(MAXIF), ACCERR(MAXIF), RPHASE(MAXIF), PHSERR(MAXIF)
      INTEGER   BASELN(2), SUBARR, STOKES, SOURCE
      LOGICAL   FLAGD
      COMMON /BSRECD/ TIME, INTERV, VAMP, SAMP, RMBD, MBDERR, MBDAMB,
     *   RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCL, ACCERR,
     *   RPHASE, PHSERR, BASELN, SUBARR, STOKES, SOURCE, FLAGD
      SAVE /BSRECD/
LOCAL END
LOCAL INCLUDE 'BSSELN.INC'
C                                        Selection criteria for BS
C                                        table records.
      INCLUDE 'INCS:PUVD.INC'
C
C                                        SRCLST = list of source
C                                                 codes requested
C                                        NSRC = number of items in
C                                               SRCLST
      INTEGER   SRCLST(XSTBSZ), NSRC
C                                        STKLST = list of requested
C                                                 Stokes codes
C                                        NSTK = number of items in
C                                               STKLST
C                                        STKIND = polarization indices
C                                                 of items from STKLST
C                                                 in SN/CL table
      INTEGER   STKLST(2), STKIND(2), NSTK
C                                        FRQID = selected frequency ID
      INTEGER   FRQID
C                                        START = earliest time wanted
C                                                in days
C                                        FINISH = last time wanted in
C                                                 days
      REAL      START, FINISH
C                                        ANLIST = list of requested
C                                                 antenna numbers
C                                        NANTS = number of items in
C                                                ANLIST
C                                        SUBARY = subarray number
C                                        REFANT = reference antenna
C                                                 (0 => none)
      INTEGER   ANLIST(50), NANTS, SUBARY, REFANT
C
      COMMON /BSSELN/ START, FINISH,
     *   SRCLST, NSRC, STKLST, STKIND, NSTK, FRQID, ANLIST, NANTS,
     *   SUBARY, REFANT
      SAVE /BSSELN/
LOCAL END
LOCAL INCLUDE 'CLNUP.INC'
C                                        Actions to take on abort.
C
C                                        ZAPSN = true if scratch
C                                                SN table should be
C                                                deleted
C                                        ZAPSBS = true if scratch BS
C                                                 table should be
C                                                 deleted
      LOGICAL   ZAPSN, ZAPSBS
      COMMON /CLNUP/ ZAPSN, ZAPSBS
      SAVE /CLNUP/
LOCAL END
LOCAL INCLUDE 'CONTRL.INC'
C                                       Control parameters
C
C                                       DOCAL = true if solutions are
C                                               to be applied
C                                       MK4 = true for MK4IN data
      LOGICAL   DOCAL, MK4
      COMMON /CONTRL/ DOCAL, MK4
      SAVE /CONTRL/
LOCAL END
LOCAL INCLUDE 'FRQTAB.INC'
C                                        Table of IF frequencies
      REAL      IFFRQ(MAXIF)
      INTEGER   NIF
      COMMON /FRQTAB/ IFFRQ, NIF
      SAVE /FRQTAB/
LOCAL END
LOCAL INCLUDE 'SCAN.INC'
C                                        Scan accumulation buffers:
C
C                                        SCNREF = reference antenna
C                                                 for scan
      INTEGER   SCNREF
C                                        NDATA(P) = number of data
C                                                   points for
C                                                   poln P.
      INTEGER   NDATA(2)
C                                        SCTIME = scan time
C                                        SCLEN = scan length
      DOUBLE PRECISION SCTIME
      REAL      SCLEN
C                                        SCNSRC = source
      INTEGER   SCNSRC
C                                        BL(2, D, P) = baseline
C                                        PH(IF, D, P) = phase
C                                        PHW(IF, D, P) = phase wt
C                                        SB(IF, D, P) = sb delay
C                                        SBW(IF, D, P) = sbd wt
C                                        RT(IF, D, P) = rate
C                                        RTW(IF, D, P) = rate wt
C                                        AC(IF, D, P) = acceln
C                                        ACW(IF, D, P) = accln. wt.
C                                        MB(D, P) = mb delay
C                                        MBW(D, P) = mbd wt
C                                        SBA(D, P) = sbd ambiguity
C                                        RTA(D, P) = rate ambiguity
C                                        MBA(D, P) = mbd ambiguity
      INTEGER   BL(2, MXBASE, 2)
      REAL      MB(MXBASE, 2), MBW(MXBASE, 2), SBA(MXBASE, 2),
     *   RTA(MXBASE,2), MBA(MXBASE, 2)
      COMMON /SCAN/ SCTIME, SCLEN, SCNREF, SCNSRC, NDATA, BL, MB, MBW,
     *   SBA, RTA, MBA
      SAVE /SCAN/
LOCAL END
LOCAL INCLUDE 'SNBUF.INC'
C                                        SN table scan buffer
C                                        now a Subroutine arg list
      INTEGER   NII, NAA, NBL, REFA(2,NII,NAA)
      REAL      MBDELY(2,NAA), CREAL(2,NII,NAA), CIMAG(2,NII,NAA),
     *   DELAY(2,NII,NAA), RATE(2,NII,NAA), WEIGHT(2,NII,NAA),
     *   PH(NII,NBL,2), PHW(NII,NBL,2), SB(NII,NBL,2), SBW(NII,NBL,2),
     *   RT(NII,NBL,2), RTW(NII,NBL,2), AC(NII,NBL,2), ACW(NII,NBL,2),
     *   DISP(2,NAA), DDISP(2,NAA)
LOCAL END
LOCAL INCLUDE 'WEIGHT.INC'
C                                       This local include contains the
C                                       antenna weighting factors
      REAL     ANTWT(MAXANT)
      COMMON /WEIGHT/ ANTWT
      SAVE /WEIGHT/
LOCAL END
      PROGRAM BLAPP
C-----------------------------------------------------------------------
C! Apply baseline-based fringe corrections. Special MK4 version of BLAPP
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2002, 2004-2005, 2012, 2014-2015
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
C   BLAPP reads fringe solutions from a BS table, solves for telescope
C   based terms and applies these to a calibration (CL) table.
C-----------------------------------------------------------------------
      INTEGER   IRET, BUFFER(256)
C
      CHARACTER INPUTS*8, SNTAB*16, UVDATA*8, BSTAB*16
      INTEGER   IERR
      PARAMETER (BSTAB = 'scratch BS table')
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CONTRL.INC'
      INCLUDE 'CLNUP.INC'
C-----------------------------------------------------------------------
      ZAPSN = .FALSE.
      ZAPSBS = .FALSE.
C                                        Read input parameters:
      CALL RDINP (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Interpolate solutions onto
C                                        a common time grid
      CALL GRID (INPUTS, UVDATA, BSTAB, IRET)
      IF (IRET .NE. 0) GO TO 990
C                                        Calculate antenna-based terms:
      CALL CALC (UVDATA, BSTAB, SNTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Apply corrections:
      IF (DOCAL) THEN
         CALL APPLY (SNTAB, UVDATA, INPUTS, IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE
         ZAPSN = .FALSE.
         END IF
C                                        Update history file:
      CALL HISTRY (UVDATA, INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Zap scratch table if
C                                        necessary:
  990 IF (ZAPSN) CALL TABZAP (SNTAB, IERR)
      IF (ZAPSBS) CALL TABZAP (BSTAB, IERR)
C
      CALL DIE (IRET, BUFFER)
C
  999 STOP
      END
      SUBROUTINE ACCUM (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Add the last row read from the BS table to the scan accumulation
C   buffers.  If the row starts a new scan first solve for and write out
C   the telescope-based parameters and clear out the accumulation
C   buffer.
C
C   Inputs:
C      SNTAB    C*(*)    SN table to write to
C      SLNTYP   C*4      Solution type
C
C   Input/Output:
C      SNROW    I        next SN table row to write
C
C   Output:
C      IRET     I        error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER SNTAB*(*), SLNTYP*4
      INTEGER   SNROW, IRET
      INCLUDE 'SNBUF.INC'
C
      INTEGER   I, NUMIF, PTYPE
      REAL      RTDPDY, BIGNUM
C
      REAL      SLAMCH
      EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSRECD.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
      INCLUDE 'WEIGHT.INC'
C-----------------------------------------------------------------------
      BIGNUM = SQRT (SLAMCH ('O'))
C                                        Record time if starting a new
C                                        scan:
      IF (SCTIME.LT.0.0D0) THEN
         SCTIME = TIME
         SCLEN = INTERV
         SCNSRC = SOURCE
         END IF
C                                        Solve if this is a new scan:
      IF ((TIME.GT.(SCTIME+(SCLEN/2))).OR.(SOURCE.NE.SCNSRC)) THEN
         CALL SOLVE (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA, MBDELY,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB,
     *      SBW, RT, RTW, AC, ACW, IRET)
         IF (IRET.NE.0) GO TO 990
         SCTIME = TIME
         SCLEN = INTERV
         SCNSRC = SOURCE
         DO 5 I = 1, NSTK
            NDATA(I) = 0
    5       CONTINUE
         END IF
C                                        Which polarization should be
C                                        used?
      DO 10 I = 1, NSTK
         IF (STOKES.EQ.STKLST(I)) PTYPE = STKIND(I)
   10    CONTINUE
C                                        How many independent IFs are
C                                        there?
      IF (SLNTYP.EQ.'INDE') THEN
         NUMIF = NIF
      ELSE
         NUMIF = 1
         END IF
C                                        Add data:
      NDATA(PTYPE) = NDATA(PTYPE) + 1
      BL(1, NDATA(PTYPE), PTYPE) = BASELN(1)
      BL(2, NDATA(PTYPE), PTYPE) = BASELN(2)
      DO 20 I = 1, NUMIF
C                                       Check the phase error field to
C                                       see whether there are solutions
C                                       for this IF.
         IF (PHSERR(I).EQ.FBLANK) THEN
            PH(I, NDATA(PTYPE), PTYPE) = 0.0
            PHW(I, NDATA(PTYPE), PTYPE) = 0.0
            SB(I, NDATA(PTYPE), PTYPE) = 0.0
            SBW(I, NDATA(PTYPE), PTYPE) = 0.0
            RT(I, NDATA(PTYPE), PTYPE) = 0.0
            RTW(I, NDATA(PTYPE), PTYPE) = 0.0
            AC(I, NDATA(PTYPE), PTYPE) = 0.0
            ACW(I, NDATA(PTYPE), PTYPE) = 0.0
         ELSE
C                                        Calculate fringe rate in
C                                        degrees per day:
            RTDPDY = 360 * 24 * 3600 * RRATE(I)
C                                        Phase is adjusted to
C                                        reference time.
            PH(I, NDATA(PTYPE), PTYPE) = RPHASE(I)
     *         - RTDPDY * (TIME - SCTIME)
C                                        Normalize phase:
            PH(I, NDATA(PTYPE), PTYPE)
     *         = MOD (PH(I, NDATA(PTYPE), PTYPE), 360.0)
            IF (PH(I, NDATA(PTYPE), PTYPE).GT.180.0)
     *         PH(I, NDATA(PTYPE), PTYPE)
     *         = PH(I, NDATA(PTYPE), PTYPE) - 360.0
            PHW(I, NDATA(PTYPE), PTYPE) = 1.0 / PHSERR(I)**2
     *         * SQRT (ANTWT(BASELN(1)) * ANTWT(BASELN(2)))
            SB(I, NDATA(PTYPE), PTYPE) = RSBD(I)
            SBW(I, NDATA(PTYPE), PTYPE) = 1.0 / SBDERR(I)**2
     *         * SQRT (ANTWT(BASELN(1)) * ANTWT(BASELN(2)))
            RT(I, NDATA(PTYPE), PTYPE) = RRATE(I)
            RTW(I, NDATA(PTYPE), PTYPE) = 1.0 / RTERR(I)**2
     *      * SQRT (ANTWT(BASELN(1)) * ANTWT(BASELN(2)))
            AC(I, NDATA(PTYPE), PTYPE) = RACCL(I)
C                                       Check for zero acceleration
C                                       error and substitute a large
C                                       weight for the inverse variance:
            IF (ACCERR(I).EQ.0.0) THEN
               ACW(I, NDATA(PTYPE), PTYPE) = BIGNUM
            ELSE
               ACW(I, NDATA(PTYPE), PTYPE) = 1.0 / ACCERR(I)**2
     *            * SQRT (ANTWT(BASELN(1)) * ANTWT(BASELN(2)))
               END IF
            END IF
   20    CONTINUE
C
      IF (MBDERR.EQ.FBLANK) THEN
         MB (NDATA(PTYPE), PTYPE) = 0.0
         MBW (NDATA(PTYPE), PTYPE) = 0.0
      ELSE
         MB(NDATA(PTYPE), PTYPE) = RMBD
         MBW(NDATA(PTYPE), PTYPE) = 1.0 / MBDERR**2
     *      * SQRT (ANTWT(BASELN(1)) * ANTWT(BASELN(2)))
         END IF
C
      SBA(NDATA(PTYPE), PTYPE) = SBDAMB
      RTA(NDATA(PTYPE), PTYPE) = RTAMB
      MBA(NDATA(PTYPE), PTYPE) = MBDAMB
C
 990  MSGTXT = 'ERROR RAISED IN ACCUM'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE APPLY (SNTAB, UVDATA, INPUTS, IRET)
C-----------------------------------------------------------------------
C   Apply the corrections in SN table SNTAB to the CL table designated
C   by UVDATA and the GAINVER adverb.
C
C   Inputs:
C      SNTAB     C*(*)       SN table object
C      UVDATA    C*(*)       UVDATA object
C      INPUTS    C*(*)       INPUTS object
C
C   Outputs:
C      IRET      I           Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER SNTAB*(*), UVDATA*(*), INPUTS*(*)
      INTEGER   IRET
C
      CHARACTER ICLTAB*16, OCLTAB*16
      PARAMETER (ICLTAB = 'input CL table')
      PARAMETER (OCLTAB = 'output CL table')
      INTEGER   ICLVER, OCLVER, DIM(3), TYPE, SNVER
      CHARACTER DUMMY
      DOUBLE PRECISION TIMRA(2), DLIMI
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'CLNUP.INC'
C-----------------------------------------------------------------------
C                                        Get the requested CL table

C                                        version numbers:
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL INGET (INPUTS, 'GAINVER', TYPE, DIM, ICLVER, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (ICLVER.LE.0) ICLVER = 1
      CALL INGET (INPUTS, 'GAINUSE', TYPE, DIM, OCLVER, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (OCLVER.LE.0) OCLVER = 2
C                                        Check that the output CL
C                                        table may be modified.
      IF (OCLVER.EQ.1) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (8)
C                                        Save SN table so that the
C                                        user may apply it with
C                                        CLCAL and tell him/her
C                                        about it:
         ZAPSN = .FALSE.
         CALL TABGET (SNTAB, 'VER', TYPE, DIM, SNVER, DUMMY, IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (MSGTXT, 1001)
         CALL MSGWRT (7)
         WRITE (MSGTXT, 1002) SNVER
         CALL MSGWRT (7)
         IRET = 1
         GO TO 990
         END IF

C                                        Create CL table objects:
      CALL UV2TAB (UVDATA, ICLTAB, 'CL', ICLVER, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UV2TAB (UVDATA, OCLTAB, 'CL', OCLVER, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Sort tables:
      WRITE (MSGTXT, 1003) 'SN'
      CALL MSGWRT (5)
      CALL TBLSRT (SNTAB, 'ANTENNA NO.', 'TIME  ', IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT, 1003) 'CL'
      CALL MSGWRT (5)
      CALL TBLSRT (ICLTAB, 'ANTENNA NO.', 'TIME  ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Update CL table:
      WRITE (MSGTXT, 1004)
      CALL MSGWRT (5)
      TIMRA(1) = DBLE(START)
      TIMRA(2) = DBLE(FINISH)
      DLIMI = 0.0D0
      CALL OSN2CL (SNTAB, ICLTAB, OCLTAB, NSRC, .TRUE., SRCLST, NSRC,
     *   .TRUE., SRCLST, TIMRA, SUBARY, FRQID, NANTS, .TRUE.,
     *   ANLIST, 0, 0, DLIMI, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT, 1005)
      CALL MSGWRT (5)
C                                        Destroy the SN table:
      CALL TABZAP (SNTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      ZAPSN = .FALSE.
      WRITE (MSGTXT, 1006)
      CALL MSGWRT (5)
C
 990  MSGTXT = 'ERROR RAISED IN APPLY'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT WRITE TO CL TABLE VERSION NUMBER 1')
 1001 FORMAT ('You can apply the fringe corrections without running')
 1002 FORMAT ('BLING again by running CLCAL using SN table ', I3)
 1003 FORMAT ('Sorting ', A2, ' table into antenna-time order')
 1004 FORMAT ('Updating CL table...')
 1005 FORMAT ('Finished')
 1006 FORMAT ('Removed temporary SN table')
      END
      SUBROUTINE CALC (UVDATA, BSTAB, SNTAB, IRET)
C-----------------------------------------------------------------------
C   Calculate antenna-based rates and delays and write them to a
C   solution table.
C
C   Input:
C      UVDATA     C*(*)       UVDATA object
C      BSTAB      C*(*)       Scratch BS table
C
C   Outputs:
C      SNTAB      C*(*)       Solution table containing antenna-based
C                             terms
C      IRET       I           Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), BSTAB*(*), SNTAB*(*)
      INTEGER   IRET
C
      INTEGER   BSROW, NUMIF
      CHARACTER SLNTYP*4
      INTEGER   NUMPOL, MAXNOD, NUMNOD, SNROW
      PARAMETER (MAXNOD = 1)
      REAL      RANOD(MAXNOD), DECNOD(MAXNOD), GMMOD
      LOGICAL   ISAPPL
C
      INTEGER   NII, NAA, NBL, REFA(2)
      REAL      MBDELY(2), CREAL(2), CIMAG(2), DELAY(2), RATE(2),
     *   WEIGHT(2), PH(2), PHW(2), SB(2), SBW(2), RT(2), RTW(2), AC(2),
     *   ACW(2), DISP(2), DDISP(2)
      LONGINT   OFFRE, OFFMB, OFFCR, OFFCI, OFFDE, OFFRA, OFFWE, NWORDS,
     *   OFFPH, OFFSB, OFFRT, OFFAC, OFPHW, OFSBW, OFRTW, OFACW, OFFDS,
     *   OFFDD
C     INTEGER   REFA(2,NII,NAA)
C     REAL      MBDELY(2,NAA), CREAL(2,NII,NAA), CIMAG(2,NII,NAA),
C    *   DELAY(2,NII,NAA), RATE(2,NII,NAA), WEIGHT(2,NII,NAA)
C    *   PH(NII,NBL,2), PHW(NII,NBL,2), SB(NII,NBL,2), SBW(NII,NBL,2),
C    *   RT(NII,NBL,2), RTW(NII,NBL,2), AC(NII,NBL,2), ACW(NII,NBL,2)

C
      INTEGER   DIM(3), I, NROWS, TYPE, VERS
      CHARACTER DUMMY
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'CLNUP.INC'
      INCLUDE 'FRQTAB.INC'
C
      DATA RANOD /0.0/
      DATA DECNOD /0.0/
C-----------------------------------------------------------------------
C                                        Create table objects:
      SNTAB = 'solution table'
      CALL UV2TAB (UVDATA, SNTAB, 'SN', 0, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Open BS table to set version
C                                       number and close it again:
      CALL OBSINI (BSTAB, 'READ', BSROW, SLNTYP, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TABCLO (BSTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Sort BS table into time order:
      CALL TBLSRT (BSTAB, 'TIME  ', 'TIME  ', IRET)
C                                       Trailing spaces in column
C                                       titles are required to avoid
C                                       ambiguity between TIME and
C                                       TIME INTERVAL.
      IF (IRET.NE.0) GO TO 990
C                                       Create memory
      NII = NIF
      NAA = NANTS
      NBL = (NAA * (NAA+1)) / 2
      NWORDS = (2 * NAA - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBDELY, OFFMB,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, DISP, OFFDS,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, DDISP, OFFDD,
     *   IRET)
      NWORDS = (2 * NAA * NII - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, REFA, OFFRE,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, CREAL, OFFCR,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, CIMAG, OFFCI,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, DELAY, OFFDE,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RATE, OFFRA,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WEIGHT, OFFWE,
     *   IRET)
      NWORDS = (2 * NBL * NII - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PH, OFFPH,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SB, OFFSB,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RT, OFFRT,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, AC, OFFAC,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, PHW, OFPHW,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SBW, OFSBW,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, RTW, OFRTW,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ACW, OFACW,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Open tables:
      NUMIF = NIF
      CALL OBSINI (BSTAB, 'READ', BSROW, SLNTYP, NUMIF, IRET)
      IF (IRET.NE.0) GO TO 990
      IF ((SLNTYP.EQ.'INDE').AND.(NUMIF.NE.NIF)) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
      NUMPOL = 1
      DO 10 I = 1, NSTK
         IF (STKIND(I).EQ.2) NUMPOL = 2
   10    CONTINUE
      NUMNOD = MAXNOD
      GMMOD = 1.0
      ISAPPL = .FALSE.
      CALL OSNINI (SNTAB, 'WRIT', SNROW, NANTS, NUMPOL, NIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        If anything fails from now
C                                        on we have an SN table to
C                                        get rid of.
      ZAPSN = .TRUE.
C                                        Let the user know the SN table
C                                        version number:
      CALL TABGET (SNTAB, 'VER', TYPE, DIM, VERS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (MSGTXT, 1001) VERS
      CALL MSGWRT (5)
C                                        Clear scan buffers:
      CALL CLRSCN
C                                        Process each BS table record:
      CALL TABGET (BSTAB, 'NROW', TYPE, DIM, NROWS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      DO 20 BSROW = 1, NROWS
         CALL DOROW (BSTAB, BSROW, SNTAB, SNROW, SLNTYP, NII, NAA, NBL,
     *      REFA(1+OFFRE), MBDELY(1+OFFMB), DISP(1+OFFDS),
     *      DDISP(1+OFFDD), CREAL(1+OFFCR), CIMAG(1+OFFCI),
     *      DELAY(1+OFFDE), RATE(1+OFFRA),WEIGHT(1+OFFWE), PH(1+OFFPH),
     *      PHW(1+OFPHW), SB(1+OFFSB), SBW(1+OFSBW), RT(1+OFFRT),
     *      RTW(1+OFRTW), AC(1+OFFAC),ACW(1+OFACW), IRET)
   20    CONTINUE
C                                        Process any scan left over
      CALL SOLVE (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA(1+OFFRE),
     *   MBDELY(1+OFFMB), DISP(1+OFFDS), DDISP(1+OFFDD), CREAL(1+OFFCR),
     *   CIMAG(1+OFFCI), DELAY(1+OFFDE), RATE(1+OFFRA), WEIGHT(1+OFFWE),
     *   PH(1+OFFPH), PHW(1+OFPHW), SB(1+OFFSB), SBW(1+OFSBW),
     *   RT(1+OFFRT), RTW(1+OFRTW), AC(1+OFFAC), ACW(1+OFACW), IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Close tables:
      CALL TABCLO (BSTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TABCLO (SNTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Check that something was done:
      IF (SNROW.EQ.1) THEN
         MSGTXT = 'DID NOT FIND ANY SOLUTIONS'
         CALL MSGWRT (8)
         IRET = 1
         END IF
C
 990  CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, ACW, OFACW, NII)
      MSGTXT = 'ERROR RAISED IN CALC'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NUMBER OF IFS IN BS TABLE DOES NOT MATCH UV FILE')
 1001 FORMAT ('Writing SN table version ', I3)
      END
      SUBROUTINE CLRSCN
C-----------------------------------------------------------------------
C   Clear scan buffers.  The number of data points accumulated is set
C   to zero and the reference antenna is cleared
C
C   Output:
C      NDATA    I(2)      Number of data points for each polarization
C                         (in common)
C      SCNREF   I         Reference antenna number (in common)
C      SCTIME   D         Scan time (in common)
C-----------------------------------------------------------------------
      INTEGER   I
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
      DO 10 I = 1, 2
         NDATA(I) = 0
   10    CONTINUE
      SCNREF = 0
      SCTIME = -999.0D0
  999 RETURN
      END
      SUBROUTINE CLRSNB (NII, NAA, NBL, REFA, MBDELY, DISP, DDISP,
     *   CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB, SBW, RT, RTW,
     *   AC, ACW)
C-----------------------------------------------------------------------
C   Clear the SN table scan buffer.
C-----------------------------------------------------------------------
      INCLUDE 'SNBUF.INC'
C
      INTEGER   NWORDS
C-----------------------------------------------------------------------
      NWORDS = 2 * NAA
      CALL RFILL (NWORDS, 0.0, MBDELY)
      CALL RFILL (NWORDS, 0.0, DISP)
      CALL RFILL (NWORDS, 0.0, DDISP)
      NWORDS = 2 * NAA * NII
      CALL FILL  (NWORDS, 0, REFA)
      CALL RFILL (NWORDS, 1.0, CREAL)
      CALL RFILL (NWORDS, 0.0, CIMAG)
      CALL RFILL (NWORDS, 0.0, DELAY)
      CALL RFILL (NWORDS, 0.0, RATE)
      CALL RFILL (NWORDS, 0.0, WEIGHT)
      END
      SUBROUTINE DOROW (BSTAB, BSROW, SNTAB, SNROW, SLNTYP, NII, NAA,
     *   NBL, REFA, MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *   WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Process row BSROW of the BS table BSTAB.
C
C   Inputs:
C      BSTAB   C*(*)       BS table
C      BSROW   I           Row to process
C      SNTAB   C*(*)       SN table to write results to
C      SLNTYP  C*4         Solution type
C
C   Input/Output:
C      SNROW   I           next SN table row to be written
C
C      IRET    I           error code: 0 - no errors
C-----------------------------------------------------------------------
      CHARACTER BSTAB*(*), SNTAB*(*), SLNTYP*4
      INTEGER   BSROW, SNROW, IRET
      INCLUDE 'SNBUF.INC'
C                                        WNTROW = true if the last BS
C                                                 table row meets the
C                                                 selection criteria
      LOGICAL   WNTROW
      EXTERNAL  WNTROW
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL RDROW (BSTAB, BSROW, SLNTYP, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (WNTROW()) THEN
C
         CALL ACCUM (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA, MBDELY,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB,
     *      SBW, RT, RTW, AC, ACW, IRET)
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN DOROW'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
      END
      SUBROUTINE FCLOSE (POL, SLNTYP, NII, NAA, NBL, REFA, MBDELY,
     *   DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB,
     *   SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Force rates and delays to close for polarization number POL
C
C   Inputs:
C      POL       I         Polarization number
C      SLNTYP    C*4       Solution type
C
C   Output:
C      IRET      I         Error code: 0 - no errors
C
C   Note: the algorithm used for forcing closure is similar to the one
C         used in the OLAF SCALP program.  One fine day I should
C         derive a proof that this does guarantee closure on all
C         triangles --- CF.
C-----------------------------------------------------------------------
      INTEGER   POL, IRET
      CHARACTER SLNTYP*4
      INCLUDE 'SNBUF.INC'
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXTRI
      PARAMETER (MAXTRI = (MAXANT * (MAXANT-1) * (MAXANT-2)) / 6)
      INTEGER   TRI(3, MAXTRI), NUMIF, TRIIDX(3), SIGN(3)
      INTEGER   I, J, K, NTRI
      LOGICAL   CHECK(3), ADJSTD(MXBASE)
      REAL      SUM, ADJUST

C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
      IF (SLNTYP.EQ.'INDE') THEN
         NUMIF = NIF
      ELSE
         NUMIF = 1
         END IF
C                                        Build a list of independent
C                                        closure triangles:
      NTRI = 0
      DO 30 I = 1, NANTS-2
         DO 20 J = I+1, NANTS-1
            DO 10 K = J+1, NANTS
               NTRI = NTRI + 1
               TRI(1, NTRI) = ANLIST(I)
               TRI(2, NTRI) = ANLIST(J)
               TRI(3, NTRI) = ANLIST(K)
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
C                                        Single band delays
      IF (SLNTYP.NE.'RATE') THEN
C                                        Loop over IFs
         DO 90 I = 1, NUMIF
C                                        Clear adjustment flags:
            DO 40 J = 1, NDATA(POL)
               ADJSTD(J) = .FALSE.
   40          CONTINUE
C                                        Examine each triangle:
            DO 80 J = 1, NTRI
C                                        Find component baselines:
               DO 50 K = 1, 3
                  CHECK(K) = .FALSE.
   50             CONTINUE
               DO 60 K = 1, NDATA(POL)
                  IF ((BL(1, K, POL).EQ.TRI(1, J))
     *               .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                     TRIIDX(1) = K
                     SIGN(1) = +1
                     CHECK(1) = .TRUE.
                  ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *                  .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                     TRIIDX(1) = K
                     SIGN(1) = -1
                     CHECK(1) = .TRUE.
                  ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *                  .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                     TRIIDX(2) = K
                     SIGN(2) = +1
                     CHECK(2) = .TRUE.
                  ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *                  .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                     TRIIDX(2) = K
                     SIGN(2) = -1
                     CHECK(2) = .TRUE.
                  ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *                  .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                     TRIIDX(3) = K
                     SIGN(3) = +1
                     CHECK(3) = .TRUE.
                  ELSE IF ((BL(1, K, POL).EQ.TRI(1, J))
     *                  .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                     TRIIDX(3) = K
                     SIGN(3) = -1
                     CHECK(3) = .TRUE.
                     END IF
   60             CONTINUE
C
               IF (CHECK(1).AND.CHECK(2).AND.CHECK(3)) THEN
                  DO 70 K = 1, 3
                     IF (.NOT.ADJSTD(TRIIDX(K))) THEN
                        SUM = SIGN(1) * SB(I, TRIIDX(1), POL)
     *                     + SIGN(2) * SB(I, TRIIDX(2), POL)
     *                     + SIGN(3) * SB(I, TRIIDX(3), POL)
                        ADJUST = SBA(TRIIDX(K), POL)
     *                     * NINT (SUM / SBA(TRIIDX(K), POL))
                        SB(I, TRIIDX(K), POL) = SB(I, TRIIDX(K), POL)
     *                     - SIGN(K) * ADJUST
                        ADJSTD(TRIIDX(K)) = .TRUE.
                        END IF
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         END IF
C                                        Rates
C                                        Loop over IFs
      DO 150 I = 1, NUMIF
C                                        Clear adjustment flags:
         DO 100 J = 1, NDATA(POL)
            ADJSTD(J) = .FALSE.
  100       CONTINUE
C                                        Examine each triangle:
         DO 140 J = 1, NTRI
C                                        Find component baselines:
            DO 110 K = 1, 3
               CHECK(K) = .FALSE.
  110          CONTINUE
            DO 120 K = 1, NDATA(POL)
               IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = +1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = -1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = +1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = -1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = +1
                  CHECK(3) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = -1
                  CHECK(3) = .TRUE.
                  END IF
  120          CONTINUE
C
            IF (CHECK(1).AND.CHECK(2).AND.CHECK(3)) THEN
               DO 130 K = 1, 3
                  IF (.NOT.ADJSTD(TRIIDX(K))) THEN
                     SUM = SIGN(1) * RT(I, TRIIDX(1), POL)
     *                  + SIGN(2) * RT(I, TRIIDX(2), POL)
     *                  + SIGN(3) * RT(I, TRIIDX(3), POL)
                     ADJUST = RTA(TRIIDX(K), POL)
     *                  * NINT (SUM / RTA(TRIIDX(K), POL))
                     RT(I, TRIIDX(K), POL) = RT(I, TRIIDX(K), POL)
     *                  - SIGN(K) * ADJUST
                     ADJSTD(TRIIDX(K)) = .TRUE.
                     END IF
  130             CONTINUE
               END IF
  140       CONTINUE
  150    CONTINUE
C                                        Phases
C                                        Loop over IFs
      DO 210 I = 1, NUMIF
C                                        Clear adjustment flags:
         DO 160 J = 1, NDATA(POL)
            ADJSTD(J) = .FALSE.
  160       CONTINUE
C                                        Examine each triangle:
         DO 200 J = 1, NTRI
C                                        Find component baselines:
            DO 170 K = 1, 3
               CHECK(K) = .FALSE.
  170          CONTINUE
            DO 180 K = 1, NDATA(POL)
               IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = +1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = -1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = +1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = -1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = +1
                  CHECK(3) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = -1
                  CHECK(3) = .TRUE.
                  END IF
  180          CONTINUE
C
            IF (CHECK(1).AND.CHECK(2).AND.CHECK(3)) THEN
               DO 190 K = 1, 3
                  IF (.NOT.ADJSTD(TRIIDX(K))) THEN
                     SUM = SIGN(1) * PH(I, TRIIDX(1), POL)
     *                  + SIGN(2) * PH(I, TRIIDX(2), POL)
     *                  + SIGN(3) * PH(I, TRIIDX(3), POL)
                     ADJUST = 360.0 * NINT (SUM / 360.0)
                     PH(I, TRIIDX(K), POL) = PH(I, TRIIDX(K), POL)
     *                  - SIGN(K) * ADJUST
                     ADJSTD(TRIIDX(K)) = .TRUE.
                     END IF
  190             CONTINUE
               END IF
  200       CONTINUE
  210    CONTINUE
C                                        Wide-band delays
      IF (SLNTYP.EQ.'MK3 ') THEN
C                                        Clear adjustment flags:
         DO 220 J = 1, NDATA(POL)
            ADJSTD(J) = .FALSE.
  220       CONTINUE
C                                        Examine each triangle:
         DO 260 J = 1, NTRI
C                                        Find component baselines:
            DO 230 K = 1, 3
               CHECK(K) = .FALSE.
  230          CONTINUE
            DO 240 K = 1, NDATA(POL)
               IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = +1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(1) = K
                  SIGN(1) = -1
                  CHECK(1) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(2, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = +1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(2, J))) THEN
                  TRIIDX(2) = K
                  SIGN(2) = -1
                  CHECK(2) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(3, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(1, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = +1
                  CHECK(3) = .TRUE.
               ELSE IF ((BL(1, K, POL).EQ.TRI(1, J))
     *            .AND.(BL(2, K, POL).EQ.TRI(3, J))) THEN
                  TRIIDX(3) = K
                  SIGN(3) = -1
                  CHECK(3) = .TRUE.
                  END IF
  240          CONTINUE
C
            IF (CHECK(1).AND.CHECK(2).AND.CHECK(3)) THEN
               DO 250 K = 1, 3
                  IF (.NOT.ADJSTD(TRIIDX(K))) THEN
                     SUM = SIGN(1) * MB(TRIIDX(1), POL)
     *                  + SIGN(2) * MB(TRIIDX(2), POL)
     *                  + SIGN(3) * MB(TRIIDX(3), POL)
                     ADJUST = MBA(TRIIDX(K), POL)
     *                  * NINT (SUM / MBA(TRIIDX(K), POL))
                     MB(TRIIDX(K), POL) = MB(TRIIDX(K), POL)
     *                  - SIGN(K) * ADJUST
                     ADJSTD(TRIIDX(K)) = .TRUE.
                     END IF
  250             CONTINUE
               END IF
  260       CONTINUE
         END IF
C
  999 RETURN
      END
      SUBROUTINE FMTIME (TM, CTM)
C-----------------------------------------------------------------------
C   Format a time given in days in the standard AIPS output format.
C
C   Input:
C      TM       D       A time in days
C
C   Output:
C      CTM      C*(*)   The formatted time.  It is the callers
C                       responsibility to make sure that CTM is long
C                       enough to hold the result (at least 12 chars)
C-----------------------------------------------------------------------
      DOUBLE PRECISION TM
      CHARACTER CTM*(*)
C
      DOUBLE PRECISION T
      INTEGER   D, H, M, S
C-----------------------------------------------------------------------
      D = INT (TM)
      T = 24.0 * (TM - D)
      H = INT (T)
      T = 60.0 * (T - H)
      M = INT (T)
      T = 60.0 * (T - M)
      S = NINT (T)
      WRITE (CTM, 1000) D, H, M, S
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3.3, '/', I2.2, ':', I2.2, ':', I2.2)
      END
      INTEGER FUNCTION FQSEL()
C-----------------------------------------------------------------------
C   Return the selected frequency group number.
C   This is a function to minimize the DSEL.INC namespace polution.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DSEL.INC'
      FQSEL = FRQSEL
      END
      SUBROUTINE HISTRY (UVDATA, INPUTS, IRET)
C-----------------------------------------------------------------------
C   Update the history file.
C
C   Inputs:
C      UVDATA       C*(*)        UVDATA object
C      INPUTS       C*(*)        INPUTS object containg adverbs
C
C   Output:
C      IRET         I            Error code: 0 - no errors
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), INPUTS*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPDATA.INC'
C-----------------------------------------------------------------------
      CALL OHTIME (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OHLIST (INPUTS, AVNAME, NPARMS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
      END
      SUBROUTINE OSN2CL (SNTAB, ICLTAB, OCLTAB, NSOUWD, DOSWNT, SOUWAN,
     *   NCALWD, DOCWNT, CALWAN, TIMRA, SUBA, FREQID, NANTSL, DOAWNT,
     *   ANTENS, INTMOD, ORIGIN, DLIMI, IRET)
C-----------------------------------------------------------------------
C   Update CL table CLTAB using Sn table SNTAB.
C
C   Inputs:
C      SNTAB    C*(*)      SN table
C      CLTAB    C*(*)      CL table
C      NSOUWD   I          Number of sources included/excluded
C      DOSWNT   L          True if sources in SOUWAN are included, false
C                          if they are excluded
C      SOUWAN   I(*)       Source numbers to include/exclude
C      NCALWD   I          Number of calibrators included/excluded
C      DOCWNT   L          True if calibrators in CALWAN are included,
C                          false if they are excluded
C      CALWAN   I(*)       List of calibrator source numbers
C      TIMRA    D(2)       First and last times (in days) to be
C                          considered
C      SUBA     I          Subarray number
C      FREQID   I          Frequency ID
C      NANTSL   I          Number of antennae included/excluded
C      DOAWNT   L          True if antennae in ANTENS are included,
C                          false if they are excluded
C      ANTENS   I(*)       List of antennae
C      INTMOD   I(*)       Phase interpolation mode
C                           0   '2PT' phasor interpolation
C                           1   'SELF' 2PT using only entries for same
C                                source
C                           2   'SELN' use phase and rate of nearest
C                                SN entry for this source
C                           3   'SIMP' simple linear interpolation
C                                use quickest route around for
C                                phase connection
C                           4   'AMBG'  use mean rates to resolve phase
C                                ambiguity then do linear interpolation
C                           5   'CUBE' as above but fit third order
C                                polynomial to fit phases and rates
C                                at SN entries.
C      ORIGIN   I          Origin of SN table, 0 => multi-source or
C                          unknown, 1 => single source file.
C      DLIMI    D          Max. interpolation time (days). 0=> no limit
C   Output:
C      IRET     I          Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER SNTAB*(*), ICLTAB*(*), OCLTAB*(*)
      INTEGER   NSOUWD, SOUWAN(*), NCALWD, CALWAN(*), SUBA, FREQID,
     *   NANTSL, ANTENS(*), INTMOD, ORIGIN, IRET
      LOGICAL   DOSWNT, DOCWNT, DOAWNT, ALLPAS, ALLSUB
      DOUBLE PRECISION TIMRA(2), DLIMI
C
      INTEGER   CLDISK, CLCNO, ICLVER, OCLVER
      INTEGER   SNDISK, SNCNO, SNVER
      CHARACTER ICLTYP*2, OCLTYP*2, SNTYPE*2
      INTEGER   CLCAT(256), SNCAT(256)
      INTEGER   ICLBUF(1024), OCLBUF(1024), SNBUF(1024)
C
      INCLUDE 'INCS:DMSG.INC'
      DATA ALLPAS, ALLSUB /2 * .FALSE./
C-----------------------------------------------------------------------
C                                        Get table information:
      CALL TBLKUP (ICLTAB, CLDISK, CLCNO, ICLTYP, ICLVER, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TBLKUP (OCLTAB, CLDISK, CLCNO, OCLTYP, OCLVER, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TBLKUP (SNTAB, SNDISK, SNCNO, SNTYPE, SNVER, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Check types:
      IF (ICLTYP.NE.'CL') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // ICLTYP // ' NOT CL'
         CALL MSGWRT (7)
         IRET = 7
         GO TO 990
         END IF
      IF (OCLTYP.NE.'CL') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // OCLTYP // ' NOT CL'
         CALL MSGWRT (7)
         IRET = 7
         GO TO 990
         END IF
      IF (SNTYPE.NE.'SN') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // SNTYPE // ' NOT CL'
         CALL MSGWRT (7)
         IRET = 7
         GO TO 990
         END IF
C                                        Get CATBLKs:
      CALL OBHGET (ICLTAB, CLCAT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OBHGET (SNTAB, SNCAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Apply corrections:
      CALL SN2CL (SNDISK, CLDISK, SNCNO, CLCNO, SNVER, ICLVER, OCLVER,
     *   SNCAT, CLCAT, NSOUWD, SOUWAN, DOSWNT, NCALWD, CALWAN, DOCWNT,
     *   TIMRA, SUBA, FREQID, NANTSL, DOAWNT, ANTENS, INTMOD, ORIGIN,
     *   ALLPAS, ALLSUB, SNBUF, ICLBUF, OCLBUF, DLIMI, IRET)
      IF (IRET.NE.0) GO TO 990
C
 990  MSGTXT = 'ERROR RAISED IN OSN2CL'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
      END
      SUBROUTINE RDANTS (INPUTS, IRET)
C-----------------------------------------------------------------------
C   Construct a list of required antenna numbers.
C
C   Input:
C      INPUTS   C*(*)   INPUTS object containing adverb values
C
C   Outputs:
C      ANLIST   I(50)   List of requested antennae (in COMMON)
C      NANTS    I       Number of items in ANLIST (in COMMON)
C      SUBARY   I       Subarray number (in COMMON)
C      REFANT   I       Reference antenna number (in COMMON)
C      IRET     I       Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   IRET
C                                        TABLE = dummy table object
      CHARACTER TABLE*5
      PARAMETER (TABLE = 'table')
C                                        ANTENS = value of ANTENNAS
C                                                 adverb
      INTEGER   ANTENS(50)
C                                        DOALL = want all antennae?
C                                        INVERT = invert sense of
C                                                 selection?
      LOGICAL   DOALL, INVERT
C                                        NA = number of antennae in
C                                             the subarray
      INTEGER   NA
C                                        NKEYS = number of keywords to
C                                                copy to TABLE
C                                        INKEY = keywords to copy to
C                                                TABLE
C                                        OUTKEY = names of keywords in
C                                                 TABLE
      INTEGER   NKEYS
      PARAMETER (NKEYS = 4)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*32
C
      INTEGER   I, IA, DIM(3), TYPE
      LOGICAL   WANTED
      CHARACTER DUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
C
      DATA INKEY  /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKEY /'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    '/
C-----------------------------------------------------------------------
C                                        Read adverb values:
      DIM(1) = 50
      DIM(2) = 1
      DIM(3) = 0
      CALL INGET (INPUTS, 'ANTENNAS', TYPE, DIM, ANTENS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 1
      CALL INGET (INPUTS, 'SUBARRAY', TYPE, DIM, SUBARY, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL INGET (INPUTS, 'REFANT', TYPE, DIM, REFANT, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Fill in default subarray value
C                                        if necessary:
      IF (SUBARY.LE.0) SUBARY = 1
C                                        Find the number of antennae
C                                        in the subarray:
      CALL TABCRE (TABLE, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IN2OBJ (INPUTS, NKEYS, INKEY, OUTKEY, TABLE, IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 2
      DIM(2) = 1
      CALL TABPUT (TABLE, 'TBLTYPE', OOACAR, DIM, I, 'AN', IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 1
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, 0, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL ANTNO (TABLE, SUBARY, NA, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TABDES (TABLE, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Check REFANT and set to default
C                                        if necessary:
      IF (.NOT.((1.LE.REFANT).AND.(REFANT.LE.NA))) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (6)
         REFANT = 0
         END IF
C                                        Check for special cases of the
C                                        ANTENNAS adverb:
      DOALL = .TRUE.
      INVERT = .FALSE.
      DO 10 I = 1, 50
         IF (ANTENS(I).NE.0) THEN
            DOALL = .FALSE.
            IF (ANTENS(I).LT.0) THEN
               INVERT = .TRUE.
               ANTENS(I) = -ANTENS(I)
               END IF
            END IF
   10    CONTINUE
C                                        Fill in the request list:
      IF (DOALL) THEN
         DO 20 IA = 1, NA
            ANLIST(IA) = IA
   20       CONTINUE
         NANTS = NA
      ELSE IF (INVERT) THEN
         NANTS = 0
         DO 40 IA = 1, NA
            WANTED = .TRUE.
            DO 30 I = 1, 50
               IF (ANTENS(I).EQ.IA) WANTED = .FALSE.
   30          CONTINUE
            IF (WANTED) THEN
               NANTS = NANTS + 1
               ANLIST(NANTS) = IA
               END IF
   40       CONTINUE
      ELSE
         NANTS = 0
         DO 50 IA = 1, 50
            IF ((ANTENS(IA).GT.0).AND.(ANTENS(IA).LE.NA)) THEN
               NANTS = NANTS + 1
               ANLIST(NANTS) = ANTENS(IA)
               END IF
   50       CONTINUE
         END IF
C                                        Check that something is
C                                        selected:
      IF (NANTS.EQ.0) THEN
         WRITE (MSGTXT, 1050)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN RDANTS'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('No reference antenna specified: will choose one for ',
     *   'each scan')
 1050 FORMAT ('NO VALID ANTENNAE SELECTED -- CHECK ANTENNAS ADVERB')
      END
      SUBROUTINE RDIFS (INPUTS, UVDATA, IRET)
C-----------------------------------------------------------------------
C   Read the list of IF frequencies.
C
C   Inputs:
C      INPUTS    C*(*)      INPUTS object containing adverb values
C
C   Input/output:
C      UVDATA    C*(*)      UVDATA object
C
C   Outputs:
C      IFFRQ     R*(MAXIF)  IF reference frequencies (in COMMON)
C      NIF       I          Number of IFs (in COMMON)
C      IRET      I          Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*)
      INTEGER   IRET
C                                        NKEYS = number of keywords to
C                                                copy to UVDATA
C                                        INKEY = keywords to copy to
C                                                UVDATA
C                                        OUTKEY = keywords in UVDATA
      INTEGER   NKEYS
      PARAMETER (NKEYS = 4)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*32
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER NCH
      DOUBLE PRECISION UVFREQ, FREQS(MAXCIF)
C
      DOUBLE PRECISION SUM
      INTEGER   I, IIF, ICH, DIM(3), NAXIS(16), TYPE
      CHARACTER DUMMY
C
      INTEGER   FQSEL
      EXTERNAL  FQSEL
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA INKEY  /'SELBAND ',   'SELFREQ ',
     *             'FREQID  ',   'SUBARRAY'/
      DATA OUTKEY /'CALEDIT.SELBAN  ', 'CALEDIT.SELFRQ  ',
     *             'CALEDIT.FRQSEL  ', 'CALEDIT.SUBARR  '/
C-----------------------------------------------------------------------
C                                        Copy frequency selection
C                                        criteria to UVDATA:
      CALL IN2OBJ (INPUTS, NKEYS, INKEY, OUTKEY, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Open UV file so that frequency
C                                        selection takes effect:
      CALL OUVOPN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Read number of IF's and
C                                        channels:
      DIM(1) = 16
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 2,  'FREQ', ICH, IRET)
      IF (IRET.NE.0) GO TO 990
      NCH = NAXIS(ICH)
      CALL UVDFND (UVDATA, 2, 'IF', IIF, IRET)
      IF ((IRET.LT.0).OR.(IRET.GT.1)) GO TO 990
      IF (IIF.GT.0) THEN
         NIF = NAXIS(IIF)
      ELSE
         NIF = 1
         IRET = 0
         END IF
C                                        Get actual frequency ID:
      FRQID = FQSEL()
C                                        Get frequency list:
      CALL UVFRQS (UVDATA, UVFREQ, FREQS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Average IF frequencies:
      I = 1
      DO 20 IIF = 1, NIF
         SUM = 0.0
         DO 10 ICH = 1, NCH
            SUM = SUM + FREQS(I)
            I = I + 1
   10       CONTINUE
         IFFRQ(IIF) = SUM / NCH
   20    CONTINUE
C                                        Close uv file:
      CALL OUVCLO (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C
 990  MSGTXT = 'ERROR RAISED IN RDIFS'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
      END
      SUBROUTINE RDINP (INPUTS, UVDATA, IRET)
C-----------------------------------------------------------------------
C   Read the input parameters for BLAPP.
C
C   Output:
C      INPUTS  C*(*)     INPUTS object containing adverb values
C      UVDATA  C*(*)     UVDATA object
C      IRET    I         Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER INPUTS*(*), UVDATA*(*)
C
      INTEGER   TYPE, DIM(3), IDUMMY
      CHARACTER DUMMY, OPCODE*4
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'CONTRL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPDATA.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      INPUTS = 'inputs'
      CALL AV2INP (PROG, NPARMS, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Fill in time selection
C                                        criteria:
      CALL RDTIME (INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Read requested polarizations:
      CALL RDPOLN (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Get the list of requested
C                                        antennae:
      CALL RDANTS (INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get the list of antenna weights:
      CALL RDWTS (INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Get the list of selected
C                                        sources:
      CALL RDSRCS (INPUTS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Read IF data:
      CALL RDIFS (INPUTS, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Read BADDISK parameter:
      CALL INGET (INPUTS, 'BADDISK', TYPE, DIM, IBAD, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Read OPCODE parameter:
      CALL INGET (INPUTS, 'OPCODE', TYPE, DIM, IDUMMY, OPCODE, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (OPCODE.EQ.'SOLV') THEN
         DOCAL = .FALSE.
      ELSE
         DOCAL = .TRUE.
         END IF
C                                       For MK4 do not do the CAL
      IF (OPCODE.EQ.'MK4 ') THEN
         MK4 = .TRUE.
         DOCAL = .FALSE.
      ELSE
         MK4 = .FALSE.
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN RDINP'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
      END
      SUBROUTINE RDPOLN (INPUTS, UVDATA, IRET)
C-----------------------------------------------------------------------
C   Read the requested polarizations.  This is done in two phases:
C   first the uv-file is examined to find out which polarizations
C   are present then the STOKES parameter is examined to find out
C   which of these are requested.
C
C   Input:
C      INPUTS     C*(*)       INPUTS object containing adverb values
C
C   Outputs:
C      NSTK       I           Number of polarization requests (in
C                             COMMON)
C      STKLST     I(2)        Stokes codes requested (-1 = RR, -2 = LL,
C                             -5 = XX, -6 = YY) (in COMMON)
C      STKIND     I(2)        Stokes indices in SN/CL records (in
C                             COMMON)
C      UVDATA     C*(*)       UVDATA object for uv file
C      IRET       I           Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*)
      INTEGER   IRET
C
      INTEGER   NAXIS(16), SAXIS, DIM(3), TYPE
      DOUBLE PRECISION CRVAL(16)
      REAL      CDELT(16), CRPIX(16)
C
      INTEGER   I, IP, S
      LOGICAL   CIRC, LIN, REQ(2)
      CHARACTER STOKES*4
      CHARACTER DUMMY
C                                        NKEY = number of keywords to
C                                               copy to UVDATA
      INTEGER   NKEY
      PARAMETER (NKEY = 8)
C
      CHARACTER INKEY(NKEY)*8, OUTKEY(NKEY)*32
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
C
      DATA INKEY  /'INNAME',           'INCLASS',
     *             'INSEQ',            'INDISK',
     *             'SELBAND ',         'SELFREQ ',
     *             'FREQID  ',         'SUBARRAY'/
      DATA OUTKEY /'FILE_NAME.NAME',   'FILE_NAME.CLASS',
     *             'FILE_NAME.IMSEQ',  'FILE_NAME.DISK',
     *             'CALEDIT.SELBAN  ', 'CALEDIT.SELFRQ  ',
     *             'CALEDIT.FRQSEL  ', 'CALEDIT.SUBARR  '/
C-----------------------------------------------------------------------
      UVDATA = 'UV data'
      CALL OUVCRE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IN2OBJ (INPUTS, NKEY, INKEY, OUTKEY, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Open UV file to load
C                                        data descriptor info:
      CALL OUVOPN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Get axis information:
      DIM(1) = 16
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'CRVAL', TYPE, DIM, CRVAL, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'CDELT', TYPE, DIM, CDELT, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'CRPIX', TYPE, DIM, CRPIX, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Find Stokes axis:
      CALL UVDFND (UVDATA, 2, 'STOKES', SAXIS, IRET)
      IF (IRET.NE.0) GO TO 990
C
      CIRC = .FALSE.
      LIN = .FALSE.
      NSTK = 0
      DO 10 IP = 1, NAXIS(SAXIS)
         S = NINT (CDELT(SAXIS) * (FLOAT (IP) - CRPIX(SAXIS))
     *             + CRVAL(SAXIS))
         IF ((S.EQ.-1).OR.(S.EQ.-2)) THEN
C                                        RR or LL present.
            IF (LIN) THEN
               WRITE (MSGTXT, 1000)
               CALL MSGWRT (8)
               GO TO 990
            ELSE
               CIRC = .TRUE.
               NSTK = NSTK + 1
               STKLST(NSTK) = S
               END IF
         ELSE IF ((S.EQ.-5).OR.(S.EQ.-6)) THEN
C                                        XX or YY present
            IF (CIRC) THEN
               WRITE (MSGTXT, 1000)
               CALL MSGWRT (8)
               GO TO 990
            ELSE
               CIRC = .TRUE.
               NSTK = NSTK + 1
               STKLST(NSTK) = S
               END IF
            END IF
   10    CONTINUE
C                                        Calculate indices into
C                                        SN/CL records:
      DO 20 I = 1, NSTK
         IF ((STKLST(I).EQ.-1) .OR. (STKLST(I).EQ.-5)) THEN
            STKIND(I) = 1
         ELSE IF ((STKLST(I).EQ.-2)
     *             .OR. (STKLST(I).EQ.-6)) THEN
            IF (NSTK.EQ.1) THEN
               STKIND(I) = 1
            ELSE
               STKIND(I) = 2
               END IF
            END IF
   20    CONTINUE

C                                        Read the STOKES adverb:
      DIM(1) = 4
      CALL INGET (INPUTS, 'STOKES', TYPE, DIM, I, STOKES, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Parse the STOKES adverb:
      REQ(1) = .FALSE.
      REQ(2) = .FALSE.
      DO 30 I = 1, 3, 2
         IF (STOKES(I:I+1).EQ.'RR') THEN
            IF (STKLST(1).EQ.-1) REQ(1) = .TRUE.
            IF (STKLST(2).EQ.-1) REQ(2) = .TRUE.
         ELSE IF (STOKES(I:I+1).EQ.'LL') THEN
            IF (STKLST(1).EQ.-2) REQ(1) = .TRUE.
            IF (STKLST(2).EQ.-2) REQ(2) = .TRUE.
         ELSE IF (STOKES(I:I+1).EQ.'XX') THEN
            IF (STKLST(1).EQ.-5) REQ(1) = .TRUE.
            IF (STKLST(2).EQ.-5) REQ(2) = .TRUE.
         ELSE IF (STOKES(I:I+1).EQ.'YY') THEN
            IF (STKLST(1).EQ.-6) REQ(1) = .TRUE.
            IF (STKLST(2).EQ.-6) REQ(2) = .TRUE.
         ELSE IF (STOKES(I:I+1).NE.'  ') THEN
            WRITE (MSGTXT, 1001) STOKES(I:I+1)
            CALL MSGWRT (6)
            END IF
   30    CONTINUE
      IF (STOKES.EQ.'    ') THEN
         REQ(1) = .TRUE.
         REQ(2) = .TRUE.
         END IF
C                                        Adjust Stokes list according
C                                        to requests:
      IF (REQ(1).AND.(.NOT.REQ(2))) THEN
         NSTK = 1
      ELSE IF (REQ(2).AND.(.NOT.REQ(1))) THEN
         NSTK = 1
         STKLST(1) = STKLST(2)
         STKIND(1) = STKIND(2)
      ELSE IF ((.NOT.REQ(1)).AND.(.NOT.REQ(2))) THEN
         WRITE (MSGTXT, 1002)
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                        Close UV file:
      CALL OUVCLO (UVDATA, IRET)
C
 990  MSGTXT = 'ERROR RAISED IN RDPOLN'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT HANDLE MIXED LINEAR AND CIRCULAR POLARIZATIONS')
 1001 FORMAT ('Ignoring request for polarization ''', A2, '''')
 1002 FORMAT ('NO POLARIZATIONS REQUESTED')
      END
      SUBROUTINE RDROW (BSTAB, BSROW, SLNTYP, IRET)
C-----------------------------------------------------------------------
C   Read row BSROW from BS table BSTAB.  The data read are stored in
C   a buffer held in common.
C
C   Inputs:
C      BSTAB     C*(*)       BS table
C      BSROW     I           row number to read
C      SLNTYP    C*4         Solution type
C
C   Output:
C      IRET      I           error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER BSTAB*(*), SLNTYP*4
      INTEGER   BSROW, IRET
C
      INTEGER   NUMIF, ROW
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'BSRECD.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (SLNTYP.EQ.'INDE') THEN
         NUMIF = NIF
      ELSE
         NUMIF = 1
         END IF
      ROW = BSROW
      FLAGD = .FALSE.
      CALL OTABBS (BSTAB, 'READ', ROW, NUMIF, TIME, INTERV, BASELN,
     *   SUBARR, STOKES, SOURCE, VAMP, SAMP, RMBD, MBDERR, MBDAMB,
     *   RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCL, ACCERR,
     *   RPHASE, PHSERR, IRET)
      IF (IRET.EQ.-1) THEN
         FLAGD = .TRUE.
         IRET = 0
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR RAISED IN RDROW'
         CALL MSGWRT (8)
         END IF
C
  999 RETURN
      END
      SUBROUTINE RDSRCS (INPUTS, IRET)
C-----------------------------------------------------------------------
C   Construct a list of required source numbers.
C
C   Input:
C      INPUTS   C*(*)     INPUTS object containing adverb values
C
C   Outputs:
C      SRCLST   I(XSTBSZ) List of requested sources (in COMMON)
C      NSRC     I         Number of items in SRCLST (in COMMON)
C      IRET     I         Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   IRET
C                                        SUTAB = source table object
      CHARACTER SUTAB*12
      PARAMETER (SUTAB = 'source table')
C                                        SOURCS = value of SOURCES
C                                                 adverb
      CHARACTER SOURCS(30)*16
C                                        DOALL = all sources wanted?
C                                        INVERT = invert selection?
      LOGICAL   DOALL, INVERT
C                                        NKEYS = number of keywords to
C                                                copy to SUTAB
C                                        INKEY = keywords to copy to
C                                                SUTAB
C                                        OUTKEY = keyword names in SUTAB
      INTEGER   NKEYS
      PARAMETER (NKEYS = 4)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*32
C
      INTEGER   SUROW, NUMIF, FREQID
      CHARACTER VELTYP*8, VELDEF*8
C
      INCLUDE 'INCS:PUVD.INC'
C
      INTEGER   IDSOUR, QUAL
      CHARACTER SOUNAM*16, CALCOD*4
      REAL      FLUX(4, MAXIF)
      DOUBLE PRECISION FREQ0(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
C
      INTEGER   I, IS, DIM(3), NROW, TYPE
      LOGICAL   WANTED
      CHARACTER TMP*16, DUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
C
      DATA INKEY  /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKEY /'NAME    ', 'CLASS   ', 'IMSEQ   ', 'DISK    '/
C-----------------------------------------------------------------------
C                                        Read adverb values:
      DIM(1) = 16
      DIM(2) = 30
      DIM(3) = 0
      CALL INGET (INPUTS, 'SOURCES', TYPE, DIM, I, SOURCS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Check special cases:
      DOALL = .TRUE.
      INVERT = .FALSE.
      DO 10 I = 1, 30
         IF (SOURCS(I).NE.' ') THEN
            DOALL = .FALSE.
            IF (SOURCS(I)(1:1).EQ.'-') THEN
               INVERT = .TRUE.
               TMP = SOURCS(I)
               SOURCS(I) = TMP(2:)
               END IF
            END IF
   10    CONTINUE
C                                        Open the source table:
      CALL TABCRE (SUTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IN2OBJ (INPUTS, NKEYS, INKEY, OUTKEY, SUTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 2
      DIM(2) = 1
      TMP = 'SU'
      CALL TABPUT (SUTAB, 'TBLTYPE', OOACAR, DIM, I, TMP, IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 1
      CALL TABPUT (SUTAB, 'VER', OOAINT, DIM, 0, TMP, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OSUINI (SUTAB, 'READ', NUMIF, VELTYP, VELDEF, FREQID, SUROW,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
      DIM(1) = 1
      CALL TABGET (SUTAB, 'NROW', TYPE, DIM, NROW, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Scan the source table:
      NSRC = 0
      DO 40 IS = 1, NROW
         SUROW = IS
         CALL OTABSU (SUTAB, 'READ', SUROW, IDSOUR, SOUNAM, QUAL,
     *      CALCOD, FLUX, FREQ0, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *      DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IRET)
         IF (IRET.NE.-1) THEN
C                                        Row is not flagged.
            IF (IRET.NE.0) GO TO 990
C                                        Determine whether source is
C                                        wanted:
            IF (DOALL) THEN
               WANTED = .TRUE.
            ELSE  IF (INVERT) THEN
               WANTED = .TRUE.
               DO 20 I = 1, 30
                  IF (SOUNAM.EQ.SOURCS(I)) WANTED = .FALSE.
   20             CONTINUE
            ELSE
               WANTED = .FALSE.
               DO 30 I = 1, 30
                  IF (SOUNAM.EQ.SOURCS(I)) WANTED = .TRUE.
   30             CONTINUE
               END IF
C
            IF (WANTED) THEN
               NSRC = NSRC + 1
               IF (NSRC.GT.XSTBSZ) THEN
                  WRITE (MSGTXT, 1030)
                  CALL MSGWRT (8)
                  IRET = 1
                  GO TO 990
                  END IF
               SRCLST(NSRC) = IDSOUR
               END IF
            END IF
   40    CONTINUE
C
      CALL TABCLO (SUTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TABDES (SUTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Check that something has been
C                                        selected:
      IF (NSRC.EQ.0) THEN
         WRITE (MSGTXT, 1040)
         CALL MSGWRT (8)
         IRET = 1
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN RDSRCS'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('NOT ENOUGH ROOM FOR SOURCE LIST - SEE AIPS MANAGER')
 1040 FORMAT ('NO VALID SOURCE SELECTIONS - CHECK SOURCES ADVERB')
      END
      SUBROUTINE RDTIME (INPUTS, IRET)
C-----------------------------------------------------------------------
C   Read requested timerange.
C
C   Inputs:
C      INPUTS    C*(*)      INPUTS object containing adverb values
C
C   Outputs:
C      START     R          Start of timerange in days (in COMMON)
C      FINISH    R          End of timerange in days (in COMMON)
C      IRET      I          Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   IRET
C                                        LONG = a time longer than any
C                                               conceiveable observation
C                                               in days
      REAL      LONG
      PARAMETER (LONG = 9999.0)
C                                        TENSEC = ten seconds as a
C                                                 fraction of a day
      REAL      TENSEC
      PARAMETER (TENSEC = 10.0 /(24.0 * 60.0 * 60.0))
      REAL      TIMRNG(8)
      REAL      DAY, HR, MIN
      INTEGER   DIM(3), TYPE
      CHARACTER DUMMY
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
C-----------------------------------------------------------------------
      DIM(1) = 8
      DIM(2) = 1
      DIM(3) = 0
      CALL INGET (INPUTS, 'TIMERANG', TYPE, DIM, TIMRNG, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Check and set start time:
      DAY = NINT (TIMRNG(1))
      IF (ABS (TIMRNG(1) - DAY) .GT. 0.1) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
C
      HR = NINT (TIMRNG(2))
      IF (ABS (TIMRNG(2) - HR) .GT. 0.1) THEN
         WRITE (MSGTXT, 1001)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
      IF (.NOT.((0 .LE. HR) .AND. (HR .LT. 24))) THEN
         WRITE (MSGTXT, 2000)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      MIN = NINT (TIMRNG(3))
      IF (ABS (TIMRNG(3) - MIN) .GT. 0.1) THEN
         WRITE (MSGTXT, 1002)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
      IF (.NOT.((0 .LE. MIN) .AND. (MIN .LT. 60))) THEN
         WRITE (MSGTXT, 2001)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      IF (.NOT.((0.0 .LE. TIMRNG(4)) .AND. (TIMRNG(4) .LT. 60.0))) THEN
         WRITE (MSGTXT, 2002)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      START = DAY + (HR + (MIN + TIMRNG(4) / 60.0) / 60.0) / 24.0
C                                        Check and set finish time:
      DAY = NINT (TIMRNG(5))
      IF (ABS (TIMRNG(5) - DAY) .GT. 0.1) THEN
         WRITE (MSGTXT, 1003)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
C
      HR = NINT (TIMRNG(6))
      IF (ABS (TIMRNG(6) - HR) .GT. 0.1) THEN
         WRITE (MSGTXT, 1004)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
      IF (.NOT.((0 .LE. HR) .AND. (HR .LT. 24))) THEN
         WRITE (MSGTXT, 2003)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      MIN = NINT (TIMRNG(7))
      IF (ABS (TIMRNG(7) - MIN) .GT. 0.1) THEN
         WRITE (MSGTXT, 1005)
         CALL MSGWRT (6)
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (6)
         END IF
      IF (.NOT.((0 .LE. MIN) .AND. (MIN .LT. 60))) THEN
         WRITE (MSGTXT, 2004)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      IF (.NOT.((0.0 .LE. TIMRNG(8)) .AND. (TIMRNG(8) .LT. 60.0))) THEN
         WRITE (MSGTXT, 2005)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 990
         END IF
C
      FINISH = DAY + (HR + (MIN + TIMRNG(8) / 60.0) / 60.0) / 24.0
C                                        Adjust, if necessary:
      IF (FINISH.LE.START) FINISH = 9999.0D0
C
 990  MSGTXT = 'ERROR RAISED IN RDTIME'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: day field of start time is not an integer')
 1001 FORMAT ('WARNING: hour field of start time is not an integer')
 1002 FORMAT ('WARNING: minute field of start time is not an integer')
 1003 FORMAT ('WARNING: day field of stop time is not an integer')
 1004 FORMAT ('WARNING: hour field of stop time is not an integer')
 1005 FORMAT ('WARNING: minute field of stop time is not an integer')
 1010 FORMAT ('         will round to nearest integer')
 2000 FORMAT ('HOUR FIELD OF START TIME IS NOT IN THE RANGE 0-23')
 2001 FORMAT ('MINUTE FIELD OF START TIME IS NOT IN THE RANGE 0-59')
 2002 FORMAT ('SECOND FIELD OF START TIME IS NOT IN THE RANGE 0.0-60.0')
 2003 FORMAT ('HOUR FIELD OF STOP TIME IS NOT IN THE RANGE 0-23')
 2004 FORMAT ('MINUTE FIELD OF STOP TIME IS NOT IN THE RANGE 0-59')
 2005 FORMAT ('SECOND FIELD OF STOP TIME IS NOT IN THE RANGE 0.0-60.0')
      END
      SUBROUTINE RDWTS (INPUTS, IRET)
C-----------------------------------------------------------------------
C   Read antenna weights
C
C   Inputs:
C      INPUTS     C*(*)        Inputs object
C
C   Outputs:
C      IRET       I            Error code: 0 => no error
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   IRET
C
      REAL      WTPARM (30)
      INTEGER   DIM(3), TYPE, I
      CHARACTER DUMMY
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'WEIGHT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Fill in the default weights:
      CALL RFILL (MAXANT, 1.0, ANTWT)
C                                       Get the value of the ANTWT
C                                       adverb:
      CALL INGET (INPUTS, 'ANTWT', TYPE, DIM, WTPARM, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Modify weights:
      DO 10 I = 1, 30
         IF (WTPARM(I).GT.0.0) THEN
            ANTWT(I) = WTPARM(I)
            END IF
   10    CONTINUE
C
 990  MSGTXT = 'ERROR RAISED IN RDWTS'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SOLVE (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Solve for the telescope-based terms for the scan currently in the
C   input buffers and write the solutions to solution table SNTAB.
C
C   Inputs:
C      SNTAB       C*(*)       Solution table object
C      SLNTYP      C*4         Solution type
C
C   Input/output:
C      SNROW       I           Next row of SN table to write
C
C   Output:
C      IRET        I           Error code: 0 - no errors
C-----------------------------------------------------------------------
      CHARACTER SNTAB*(*), SLNTYP*4
      INTEGER   SNROW, IRET
      INCLUDE 'SNBUF.INC'
C
      INTEGER   POL, IF, I, NA
      LOGICAL   HAVDAT
C
      INCLUDE 'INCS:PUVD.INC'
C
C   COLMAP     I(MAXANT) Mapping from antenna number to column in
C                        coefficient matrix (note that the reference
C                        antenna is excluded)
C   MANT       I         Largest antenna number
C
      INTEGER   COLMAP(MAXANT), MANT
C
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clear SN table buffer:
      CALL CLRSNB (NII, NAA, NBL, REFA, MBDELY, DISP, DDISP, CREAL,
     *   CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW)
      HAVDAT = .FALSE.
C                                       Solve for each polarization:
      DO 40 POL = 1, NSTK
C                                       Do nothing if there is no data:
         IF (NDATA(POL).GT.0) THEN
            HAVDAT = .TRUE.
C                                       Force closure:
            CALL FCLOSE (POL, SLNTYP, NII, NAA, NBL, REFA, MBDELY,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW,
     *         SB, SBW, RT, RTW, AC, ACW, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Choose a reference antenna:
            SCNREF = 0
            DO 10 I = 1, NDATA(POL)
               IF ((BL(1, I, POL).EQ.REFANT)
     *             .OR.(BL(2, I, POL).EQ.REFANT)) THEN
                  SCNREF = REFANT
                  END IF
   10          CONTINUE
C                                       If the user-designated reference
C                                       is not present use the first
C                                       antenna that is available:
            IF (SCNREF.EQ.0) SCNREF = BL(1, 1, POL)
C                                       Map antennas to columns:
            NA = 0
            MANT = SCNREF
            CALL FILL (MAXANT, 0, COLMAP)
            DO 20 I = 1, NDATA(POL)
               IF ((BL(1, I, POL).NE.SCNREF)
     *             .AND.(COLMAP(BL(1, I, POL)).EQ.0)) THEN
                  NA = NA + 1
                  COLMAP(BL(1, I, POL)) = NA
                  IF (BL(1, I, POL).GT.MANT) MANT = BL(1, I, POL)
                  END IF
               IF ((BL(2, I, POL).NE.SCNREF)
     *             .AND.(COLMAP(BL(2, I, POL)).EQ.0)) THEN
                  NA = NA + 1
                  COLMAP(BL(2, I, POL)) = NA
                  IF (BL(2, I, POL).GT.MANT) MANT = BL(2, I, POL)
                  END IF
   20          CONTINUE
            DO 30 IF = 1, NIF
               CALL SOLVPH (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL,
     *            REFA, MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
               IF (IRET.NE.0) GO TO 990
               CALL SOLVRT (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL,
     *            REFA, MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
               IF (IRET.NE.0) GO TO 990
               IF (SLNTYP.NE.'RATE') THEN
                  CALL SOLVSB (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL,
     *               REFA, MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY,
     *               RATE, WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW,
     *               IRET)
                  IF (IRET.NE.0) GO TO 990
                  END IF
   30          CONTINUE
            IF (SLNTYP.EQ.'MK3 ') THEN
               CALL SOLVMB (COLMAP, NA, MANT, POL, NII, NAA, NBL, REFA,
     *            MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, PH, PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
            END IF
   40    CONTINUE
C                                        Write out the solutions:
      IF (HAVDAT) THEN
         CALL WRTSNB (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA, MBDELY,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH, PHW, SB,
     *      SBW, RT, RTW, AC, ACW, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN SOLVE'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
  999 RETURN
      END
      SUBROUTINE SOLVPH (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Solve for residual phase in a single IF for the given polarization.
C
C   Input:
C      COLMAP   I(*)      Mapping from antenna number to column in
C                         coefficient matrix (note that the reference
C                         antenna is excluded)
C      NA       I         Number of items in COLMAP
C      MANT     I         Maximum antenna number for this scan.
C      IF       I         IF number.
C      POL      I         Polarization number
C
C   Output:
C      IRET     I       Error code: 0 - no errors
C-----------------------------------------------------------------------
      INCLUDE 'SNBUF.INC'
      INTEGER   COLMAP(*), NA, MANT, IF, POL, IRET
C
      INCLUDE 'INCS:PUVD.INC'
C                                        A = linear equation
C                                        coefficients
C                                        B = LHS/solution matrix
      REAL      A(MXBASE, MAXANT), B(MXBASE)
C
      INTEGER   RANK, LWORK, INFO, JPVT(MAXANT)
      PARAMETER (LWORK = 4 * MAXANT)
      REAL      WORK(LWORK), RCOND
C
      INTEGER   I
C
      REAL      SLAMCH
      EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
C                                        Clear A:
      CALL RFILL (MAXANT * MXBASE, 0.0, A)
C                                        Load the data:
      DO 10 I = 1, NDATA(POL)
         IF (BL(1, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(1, I, POL))) = PHW(IF, I, POL)
            END IF
         IF (BL(2, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(2, I, POL))) = -PHW(IF, I, POL)
            END IF
         B(I) = PH(IF, I, POL) * PHW(IF, I, POL)
   10 CONTINUE
C                                        Use a reasonable condition
C                                        number:
      RCOND = NDATA(POL) * SLAMCH ('epsilon')
C                                        Solve for telescope terms:
      CALL FILL (MAXANT, 0, JPVT)
      CALL SGELSX (NDATA(POL), NA, 1, A, MXBASE, B, MXBASE, JPVT, RCOND,
     *   RANK, WORK, INFO)
      IF (INFO.LT.0) THEN
         WRITE (MSGTXT, 1010) -INFO
         CALL MSGWRT (8)
         IRET = -INFO
         GO TO 990
      ELSE IF (INFO.NE.0) THEN
C                                        SVD algorithm did not converge
         DO 20 I = 1, MANT
            REFA(POL, IF, I) = 0
            CREAL(POL, IF, I) = FBLANK
            CIMAG(POL, IF, I) = FBLANK
            WEIGHT(POL, IF, I) = 0.0
   20       CONTINUE
         IRET = 0
      ELSE
C                                        Convergence achieved -- sort
C                                        out the results:
         REFA(POL, IF, SCNREF)  = SCNREF
         CREAL(POL, IF, SCNREF) = 1.0
         CIMAG(POL, IF, SCNREF) = 0.0
         WEIGHT(POL, IF, SCNREF) = 1.0
         DO 30 I = 1, MANT
            IF (COLMAP(I) .GT. 0) THEN
C                                       Data was present for antenna I.
               IF (JPVT(COLMAP(I)).LE.RANK) THEN
                  REFA(POL, IF, I)  = SCNREF
                  CREAL(POL, IF, I) = COS (DG2RAD * B(COLMAP(I)))
                  CIMAG(POL, IF, I) = SIN (DG2RAD * B(COLMAP(I)))
                  WEIGHT(POL, IF, I) = 1.0
               ELSE
C                                       The antenna was disconnected
C                                       from the reference.
                  REFA(POL, IF, I) = 0
                  CREAL(POL, IF, I) = FBLANK
                  CIMAG(POL, IF, I) = FBLANK
                  WEIGHT(POL, IF, I) = 0.0
                  END IF
            ELSE IF (I .NE. SCNREF) THEN
C                                       No data for this antenna
               REFA(POL, IF, I) = 0
               CREAL(POL, IF, I) = FBLANK
               CIMAG(POL, IF, I) = FBLANK
               WEIGHT(POL, IF, I) = 0.0
               END IF
   30       CONTINUE
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN SOLVPH'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN CALL SEQUENCE FOR SGELSX: ARGUMENT ', I2)
      END
      SUBROUTINE SOLVRT (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Solve for residual fringe rate in a single IF for the given
C   polarization.
C
C   Input:
C      COLMAP   I(*)      Mapping from antenna number to column in
C                         coefficient matrix (note that the reference
C                         antenna is excluded)
C      NA       I         Number of items in COLMAP
C      MANT     I         Maximum antenna number for this scan.
C      IF       I         IF number.
C      POL      I         Polarization number
C
C   Output:
C      IRET     I       Error code: 0 - no errors
C-----------------------------------------------------------------------
      INTEGER   COLMAP(*), NA, MANT, IF, POL, IRET
      INCLUDE 'SNBUF.INC'
C
      INCLUDE 'INCS:PUVD.INC'
C                                        A = linear equation
C                                        coefficients
C                                        B = LHS/solution matrix
      REAL      A(MXBASE, MAXANT), B(MXBASE)
C
      INTEGER   RANK, LWORK, INFO, JPVT(MAXANT)
      PARAMETER (LWORK = 4 * MAXANT)
      REAL      WORK(LWORK), RCOND
C
      INTEGER   I
C
      REAL      SLAMCH
      EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
C                                        Clear A:
      CALL RFILL (MAXANT * MXBASE, 0.0, A)
C                                        Load the data:
      DO 10 I = 1, NDATA(POL)
         IF (BL(1, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(1, I, POL))) = RTW(IF, I, POL)
            END IF
         IF (BL(2, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(2, I, POL))) = -RTW(IF, I, POL)
            END IF
         B(I) = RT(IF, I, POL) * RTW(IF, I, POL)
 10      CONTINUE
C                                        Use a reasonable condition
C                                        number:
      RCOND = NDATA(POL) * SLAMCH ('epsilon')
C                                        Solve for telescope terms:
      CALL FILL (MAXANT, 0, JPVT)
      CALL SGELSX (NDATA(POL), NA, 1, A, MXBASE, B, MXBASE, JPVT, RCOND,
     *   RANK, WORK, INFO)
      IF (INFO.LT.0) THEN
         WRITE (MSGTXT, 1010) -INFO
         CALL MSGWRT (8)
         IRET = -INFO
         GO TO 990
      ELSE IF (INFO.NE.0) THEN
C                                        SVD algorithm did not converge
         DO 20 I = 1, MANT
            RATE(POL, IF, I) = FBLANK
            WEIGHT(POL, IF, I) = 0.0
   20       CONTINUE
         IRET = 0
      ELSE
C                                        Convergence achieved -- sort
C                                        out the results:
         REFA(POL, IF, SCNREF) = SCNREF
         RATE(POL, IF, SCNREF) = 0.0
         DO 30 I = 1, MANT
            IF (COLMAP(I) .GT. 0) THEN
C                                       Data available for antenna I
               IF (JPVT(COLMAP(I)).LE.RANK) THEN
C                                       Watch for previously flagged
C                                       entries.
                  IF (WEIGHT(POL, IF, I).GT.0.0) THEN
                     RATE(POL, IF, I) = B(COLMAP(I)) / IFFRQ(IF)
                  ELSE
                     RATE(POL, IF, I) = FBLANK
                     END IF
               ELSE
C                                       The antenna was disconnected
C                                       from the reference.
                  REFA(POL, IF, I) = 0
                  RATE(POL, IF, I) = FBLANK
                  WEIGHT(POL, IF, I) = 0.0
                  END IF
            ELSE IF (I .NE. SCNREF) THEN
C                                       No data for antenna I
               REFA(POL, IF, I) = 0
               RATE(POL, IF, I) = FBLANK
               WEIGHT(POL, IF, I) = 0.0
               END IF
   30       CONTINUE
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN SOLVRT'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN CALL SEQUENCE FOR SGELSX: ARGUMENT ', I2)
      END
      SUBROUTINE SOLVSB (COLMAP, NA, MANT, IF, POL, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Solve for residual single-band delay in a single IF for the given
C   polarization.
C
C   Input:
C      COLMAP   I(*)      Mapping from antenna number to column in
C                         coefficient matrix (note that the reference
C                         antenna is excluded)
C      NA       I         Number of items in COLMAP
C      MANT     I         Maximum antenna number for this scan.
C      IF       I         IF number.
C      POL      I         Polarization number
C
C   Output:
C      IRET     I       Error code: 0 - no errors
C-----------------------------------------------------------------------
      INTEGER   COLMAP(*), NA, MANT, IF, POL, IRET
      INCLUDE 'SNBUF.INC'
C
      INCLUDE 'INCS:PUVD.INC'
C                                        A = linear equation
C                                        coefficients
C                                        B = LHS/solution matrix
      REAL      A(MXBASE, MAXANT), B(MXBASE)
C
      INTEGER   RANK, LWORK, INFO, JPVT(MAXANT)
      PARAMETER (LWORK = 4 * MAXANT)
      REAL      WORK(LWORK), RCOND
C
      INTEGER   I
C
      REAL      SLAMCH
      EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
C                                        Clear A:
      CALL RFILL (MAXANT * MXBASE, 0.0, A)
C                                        Load the data:
      DO 10 I = 1, NDATA(POL)
         IF (BL(1, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(1, I, POL))) = SBW(IF, I, POL)
            END IF
         IF (BL(2, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(2, I, POL))) = -SBW(IF, I, POL)
            END IF
         B(I) = SB(IF, I, POL) * SBW(IF, I, POL)
   10 CONTINUE
C                                        Use a reasonable condition
C                                        number:
      RCOND = NDATA(POL) * SLAMCH ('epsilon')
C                                        Solve for telescope terms:
      CALL FILL (MAXANT, 0, JPVT)
      CALL SGELSX (NDATA(POL), NA, 1, A, MXBASE, B, MXBASE, JPVT, RCOND,
     *   RANK, WORK, INFO)
      IF (INFO.LT.0) THEN
         WRITE (MSGTXT, 1010) -INFO
         CALL MSGWRT (8)
         IRET = -INFO
         GO TO 990
      ELSE IF (INFO.NE.0) THEN
C                                        SVD algorithm did not converge
         DO 20 I = 1, MANT
            DELAY(POL, IF, I) = FBLANK
            WEIGHT(POL, IF, I) = 0.0
   20       CONTINUE
         IRET = 0
      ELSE
C                                        Convergence achieved -- sort
C                                        out the results:
         REFA(POL, IF, SCNREF) = SCNREF
         DELAY(POL, IF, SCNREF) = 0.0
         DO 30 I = 1, MANT
            IF (COLMAP(I) .GT. 0) THEN
C                                       Data is present for antenna I
               IF (JPVT(COLMAP(I)).LE.RANK) THEN
C                                       Watch for previously flagged
C                                       entries.
                  IF (WEIGHT(POL, IF, I).GT.0.0) THEN
                     DELAY(POL, IF, I) = B(COLMAP(I))
                  ELSE
                     DELAY(POL, IF, I) = FBLANK
                     END IF
               ELSE
C                                       The antenna was disonnected from
C                                       the reference
                  REFA(POL, IF, I) = 0
                  DELAY(POL, IF, I) = FBLANK
                  WEIGHT(POL, IF, I) = 0.0
                  END IF
            ELSE IF (I .NE. SCNREF) THEN
C                                       No data for antenna I
               REFA(POL, IF, I) = 0
               DELAY(POL, IF, I) = FBLANK
               WEIGHT(POL, IF, I) = 0.0
               END IF
   30       CONTINUE
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN SOLVSB'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN CALL SEQUENCE FOR SGELSX: ARGUMENT ', I2)
      END
      SUBROUTINE SOLVMB (COLMAP, NA, MANT, POL, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Solve for residual multiband delay for the given polarization.
C
C   Input:
C      COLMAP   I(*)      Mapping from antenna number to column in
C                         coefficient matrix (note that the reference
C                         antenna is excluded)
C      NA       I         Number of items in COLMAP
C      MANT     I         Maximum antenna number for this scan.
C      POL      I         Polarization number
C
C   Output:
C      IRET     I       Error code: 0 - no errors
C-----------------------------------------------------------------------
      INTEGER   COLMAP(*), NA, MANT, POL, IRET
      INCLUDE 'SNBUF.INC'
C
      INCLUDE 'INCS:PUVD.INC'
C                                        A = linear equation
C                                        coefficients
C                                        B = LHS/solution matrix
      REAL      A(MXBASE, MAXANT), B(MXBASE)
C
      INTEGER   RANK, LWORK, INFO, JPVT(MAXANT)
      PARAMETER (LWORK = 4 * MAXANT)
      REAL      WORK(LWORK), RCOND, PHAS
C
      INTEGER   I, J
C
      REAL      SLAMCH
      EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
C                                        Clear A:
      CALL RFILL (MAXANT * MXBASE, 0.0, A)
C                                        Load the data:
      DO 10 I = 1, NDATA(POL)
         IF (BL(1, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(1, I, POL))) = MBW(I, POL)
            END IF
         IF (BL(2, I, POL).NE.SCNREF) THEN
            A(I, COLMAP(BL(2, I, POL))) = -MBW(I, POL)
            END IF
         B(I) = MB(I, POL) * MBW(I, POL)
   10 CONTINUE
C                                        Use a reasonable condition
C                                        number:
      RCOND = NDATA(POL) * SLAMCH ('epsilon')
C                                        Solve for telescope terms:
      CALL FILL (MAXANT, 0, JPVT)
      CALL SGELSX (NDATA(POL), NA, 1, A, MXBASE, B, MXBASE, JPVT, RCOND,
     *   RANK, WORK, INFO)
      IF (INFO.LT.0) THEN
         WRITE (MSGTXT, 1010) -INFO
         CALL MSGWRT (8)
         IRET = -INFO
         GO TO 990
      ELSE IF (INFO.NE.0) THEN
C                                        SVD algorithm did not converge
         DO 20 I = 1, MANT
            MBDELY(POL, I) = FBLANK
            DISP(POL, I) = FBLANK
            DDISP(POL, I) = FBLANK
            WEIGHT(POL, 1, I) = 0.0
   20       CONTINUE
         IRET = 0
      ELSE
C                                        Convergence achieved -- sort
C                                        out the results:
         REFA(POL, 1, SCNREF) = SCNREF
         MBDELY(POL, SCNREF) = 0.0
         DISP(POL, SCNREF) = 0.0
         DDISP(POL, SCNREF) = 0.0
         DO 40 I = 1, MANT
            IF (COLMAP(I) .GT. 0) THEN
C                                       Data available for antenna I
               IF (JPVT(COLMAP(I)).LE.RANK) THEN
C                                       Watch for previously flagged
C                                       entries.
                  IF (WEIGHT(POL, 1, I).GT.0.0) THEN
                     MBDELY(POL, I) = B(COLMAP(I))
                     DO 30 J = 1, NIF
                        PHAS = ATAN2 (CIMAG(POL, J, I),
     *                     CREAL(POL, J, I))
     *                     + TWOPI * IFFRQ(J) * MBDELY(POL, I)
                        CREAL(POL, J, I) = COS (PHAS)
                        CIMAG(POL, J, I) = SIN (PHAS)
   30                   CONTINUE
                  ELSE
                     MBDELY(POL, I) = FBLANK
                     END IF
               ELSE
C                                       The antenna was disconnected
C                                       from the reference.
                  REFA(POL, 1, I) = 0
                  MBDELY(POL, I) = FBLANK
                  WEIGHT(POL, 1, I) = 0.0
                  END IF
            ELSE IF (I .NE. SCNREF) THEN
C                                       No data for antenna I
               REFA(POL, 1, I) = 0
               MBDELY(POL, I) = FBLANK
               WEIGHT(POL, 1, I) = 0.0
               END IF
   40       CONTINUE
         END IF
C
 990  MSGTXT = 'ERROR RAISED IN SOLVMB'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN CALL SEQUENCE FOR SGELSX: ARGUMENT ', I2)
      END
      LOGICAL FUNCTION WNTROW()
C-----------------------------------------------------------------------
C   Return true if the BS table row last read meets the selection
C   criteria.
C-----------------------------------------------------------------------
      INTEGER   I
      LOGICAL   MATCH1, MATCH2
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BSRECD.INC'
      INCLUDE 'BSSELN.INC'
C-----------------------------------------------------------------------
      WNTROW = .TRUE.
      IF (FLAGD) THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
C                                        Check time range:
      IF (TIME.LT.START) THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
      IF (TIME.GT.FINISH) THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
C                                        Check subarray:
      IF ((SUBARR.GT.0) .AND. (SUBARY.GT.0) .AND. (SUBARR.NE.SUBARY))
     *   THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
C                                        Check antennae:
      MATCH1 = .FALSE.
      MATCH2 = .FALSE.
      DO 10 I = 1, NANTS
         IF (BASELN(1).EQ.ANLIST(I)) MATCH1 = .TRUE.
         IF (BASELN(2).EQ.ANLIST(I)) MATCH2 = .TRUE.
   10    CONTINUE
      IF (.NOT.(MATCH1.AND.MATCH2)) THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
C                                        Check Stokes code:
      MATCH1 = .FALSE.
      DO 20 I = 1, NSTK
         IF (STOKES.EQ.STKLST(I)) MATCH1 = .TRUE.
   20    CONTINUE
      IF (.NOT.MATCH1) THEN
         WNTROW = .FALSE.
         GO TO 999
         END IF
C                                        Check source ID:
      IF (SOURCE.NE.0) THEN
         MATCH1 = .FALSE.
         DO 30 I = 1, NSRC
            IF (SOURCE.EQ.SRCLST(I)) MATCH1 = .TRUE.
   30       CONTINUE
         IF (.NOT.MATCH1) THEN
            WNTROW = .FALSE.
            GO TO 999
         END IF
      END IF
C
  999 RETURN
      END
      SUBROUTINE WRTSNB (SNTAB, SNROW, SLNTYP, NII, NAA, NBL, REFA,
     *   MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, PH,
     *   PHW, SB, SBW, RT, RTW, AC, ACW, IRET)
C-----------------------------------------------------------------------
C   Write out the records currently in the SN table buffer.
C
C   Inputs:
C      SNTAB       C*(*)       The SN table object
C      SLNTYP      C*4         Solution type
C
C   Input/output:
C      SNROW       I           The next SN table row to be written
C
C      IRET        I           Error code: 0 - no errors detected
C-----------------------------------------------------------------------
      CHARACTER SNTAB*(*), SLNTYP*4
      INTEGER   SNROW, IRET
      INCLUDE 'SNBUF.INC'
C
      INTEGER   I, J
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSSELN.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'SCAN.INC'
C-----------------------------------------------------------------------
      DO 20 I = 1,NAA
         IF (SLNTYP.NE.'INDE') THEN
            DO 10 J = 2, NIF
               DELAY(1, J, I) = DELAY(1, 1, I)
               DELAY(2, J, I) = DELAY(2, 1, I)
               RATE(1, J, I) = RATE(1, 1, I)
               RATE(2, J, I) = RATE(2, 1, I)
   10          CONTINUE
            END IF
         IF (REFA(1, 1, I).GT.0) THEN
            CALL OTABSN (SNTAB, 'WRIT', SNROW, NSTK, SCTIME, SCLEN,
     *         SCNSRC,I, SUBARY, FRQID, 0.0, 1, MBDELY(1, I),
     *         DISP(1, I), DDISP(1, I), CREAL(1, 1, I), CIMAG(1, 1, I),
     *         DELAY(1, 1, I), RATE(1, 1, I),WEIGHT(1, 1, I),
     *         REFA(1, 1, I), IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR RAISED IN WRTSNB'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
   20    CONTINUE
C
  999 RETURN
      END
      SUBROUTINE GRID (INPUTS, UVDATA, BSTAB, IRET)
C-----------------------------------------------------------------------
C   Interpolate the solutions in the input BS table (specified by
C   version number INVERS) to the times listed in the CL table specified
C   by GAINVER and write the interpolated values to a scratch BS
C   table BSTAB.  All tables are attached to UVDATA.
C
C   Inputs:
C      INPUTS    C*(*)    INPUTS object used to access adverb values
C      UVDATA    C*(*)    UVDATA object used to access uv data file
C      BSTAB     C*(*)    TABLE object used to access scratch BS table
C
C   Output:
C      IRET      I        Error code (0 implies no errors detected)
C
C   Preconditions:
C      INPUTS and UVDATA are initialized
C      BSTAB is not initialized
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*), BSTAB*(*)
      INTEGER   IRET
C
C   Local variables:
C      INTAB     TABLE object used to access input BS table
C      TIMES     List of times found in the CL table in ascending
C                order
C      MAXTIM    Maximum number of entries in TIMES
C      NTIMES    Actual number of entries in TIMES
C                (1 <= NTIMES <= MAXTIM)
C      LOWIND    First entry in TIMES greater than or equal to the
C                start time of the current scan; used to approximate
C                the first entry for the next scan.
C                (1 <= LOWIND <= NTIMES + 1)
C      TINDEX    Index into the TIMES array (1 <= TINDEX <= NTIMES)
C      NROWS     Number of rows in the BS table
C      MBDSLN    Does the BS table contain multiband solutions?
C      NUMIF     Number of IF entries in the BS table
C      INREC     Next input record number
C      OUTREC    Next output record number
C      IMBD      Interpolated multiband-delay
C      IMBDER    Interpolated multiband delay error
C      ISBD      Interpolated single-band delays
C      ISBDER    Interpolated single-band delay errors
C      IRT       Interpolated rates
C      IRTERR    Interpolated rate errors
C      IPHASE    Interpolated phases
C      INVER     Value of INVERS keyword
C      SORT      BS table sort keys
C      TYPE      Attribute type code
C      DIM       Attribute dimensions
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER INTAB*16, CDUMMY
      INTEGER   MAXTIM
      PARAMETER (MAXTIM = 20000)
      PARAMETER (INTAB = 'input BS table')
      DOUBLE PRECISION TIMES(MAXTIM), DT
      INTEGER   NTIMES, LOWIND, TINDEX, NROWS, NUMIF, INREC, OUTREC,
     *   INVER, SORT(2), TYPE, DIM(3), I
      LOGICAL   MBDSLN
      REAL      IMBD, IMBDER, ISBD(MAXIF), ISBDER(MAXIF), IRT(MAXIF),
     *   IRTERR(MAXIF), IPHASE(MAXIF)
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSRECD.INC'
      INCLUDE 'CLNUP.INC'
      INCLUDE 'FRQTAB.INC'
      INCLUDE 'CONTRL.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Interpolating solutions onto a common time grid'
      IF (.NOT.MK4) CALL MSGWRT (4)
C
C     Read the times from the input CL table:
C
      CALL RDTIMS (INPUTS, UVDATA, MAXTIM, TIMES, NTIMES, IRET)
      IF (IRET .NE. 0) GO TO 990
C
C     Open the input BS table for reading and find the number of rows
C
      CALL INGET (INPUTS, 'INVERS', TYPE, DIM, INVER, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL UV2TAB (UVDATA, INTAB, 'BS', INVER, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL OBSINI (INTAB, 'READ', INREC, MBDSLN, NUMIF, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'CAN NOT OPEN INPUT BS TABLE (CHECK ADVERBS)'
         CALL MSGWRT (9)
         GO TO 990
      END IF
      CALL TABGET (INTAB, 'NROW', TYPE, DIM, NROWS, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
      IF (NROWS .EQ. 0) THEN
         MSGTXT = 'INPUT BS TABLE IS EMPTY - ABORTING'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 990
      END IF
C
C     Make sure that the input table is in time order
C
      CALL TABGET (INTAB, 'SORT', TYPE, DIM, SORT, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
C
C     Sort the table if it is not in time order
C
      IF (SORT(1) .NE. 1) THEN
         CALL TABCLO (INTAB, IRET)
         IF (IRET .NE. 0) GO TO 990
C                                       Mod for MK4IN BS table
         IF (MK4) THEN
            CALL TBLSRT (INTAB, 'TIME    ', 'SOURCE  ', IRET)
         ELSE
            CALL TBLSRT (INTAB, 'TIME    ', 'TIME    ', IRET)
            ENDIF
         IF (IRET .NE. 0) GO TO 990
         CALL TABOPN (INTAB, 'READ', IRET)
         IF (IRET .NE. 0) GO TO 990
      END IF

C
C     Create and open a scratch BS table and mark it for destruction
C     if an error occurs:
C
      CALL UV2TAB (UVDATA, BSTAB, 'BS', 0, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL OBSINI (BSTAB, 'WRIT', OUTREC, MBDSLN, NUMIF, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'CAN NOT CREATE OR OPEN A SCRATCH TABLE'
         CALL MSGWRT (9)
         GO TO 990
      END IF
      ZAPSBS = .TRUE.
C
      LOWIND = 1
C
C     Simulated while loop that processes each record in the input
C     table.
C     Invariant: input records 1 to INREC - 1 have been processed
C     Bound: NROWS - INREC + 1
C
   10 IF (INREC .LE. NROWS) THEN
         CALL OTABBS (INTAB, 'READ', INREC, NUMIF, TIME, INTERV,
     *      BASELN, SUBARR, STOKES, SOURCE, VAMP, SAMP, RMBD, MBDERR,
     *      MBDAMB, RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCL,
     *      ACCERR, RPHASE, PHSERR, IRET)
         IF (IRET .GT. 0) GO TO 990
C
C        Process the record if it is not flagged:
C
         IF (IRET .EQ. 0) THEN
C                                       Don't use grid for MK4IN data
            IF (.NOT.MK4) THEN
C
C           Find the first grid time in the solution interval
C           Simulated while loop with two branches.
C           Invariant: (TIMES(1:LOWIND-1) < TIME - INTERV/2
C                      or (TIMES(LOWIND:NTIMES) >= TIME - INTERV/2
C
   20       IF (LOWIND .LE. NTIMES) THEN
               IF (TIMES(LOWIND) .LT. (TIME - 0.5 * INTERV)) THEN
                  LOWIND = LOWIND + 1
                  GO TO 20
               END IF
            ELSE IF (LOWIND .GT. 1) THEN
               IF (TIMES(LOWIND-1) .GE. (TIME - 0.5 * INTERV)) THEN
                  LOWIND = LOWIND - 1
                  GO TO 20
               END IF
            END IF
C
            TINDEX = LOWIND
C
C           Write out interpolated solutions for entries in TIMES
C           covering the time interval of the solution.
C
   30       IF (TINDEX .LE. NTIMES) THEN
               IF (TIMES(TINDEX) .LE. (TIME + 0.5 * INTERV)) THEN
                  DT = (TIMES(TINDEX) - TIME) / (24.0 * 3600.0)
                  IF (MBDERR .NE. FBLANK) THEN
                     IMBD = RMBD + RRATE(1) * DT / IFFRQ(1)
     *                  + 0.5 * RACCL(1) * DT**2 / IFFRQ(1)
                     IMBDER = MBDERR + RTERR(1) * DT / IFFRQ(1)
     *                  + 0.5 * ACCERR(1) * DT**2 / IFFRQ(1)
                  ELSE
                     IMBD = FBLANK
                     IMBDER = FBLANK
                  END IF
                  DO 40 I = 1, NUMIF
                     IF (SBDERR(I) .NE. FBLANK) THEN
                        ISBD(I) = RSBD(I) + RRATE(I) * DT / IFFRQ(I)
     *                     + 0.5 * RACCL(I) * DT**2 / IFFRQ(I)
                        ISBDER(I) = SBDERR(I) + RTERR(I) * DT / IFFRQ(I)
     *                     + 0.5 * ACCERR(I) * DT**2 / IFFRQ(I)
                        IRT(I) = RRATE(I) + RACCL(I) * DT / IFFRQ(I)
                        IRTERR(I) = RTERR(I) + ACCERR(I) * DT / IFFRQ(I)
                        IPHASE(I) = RPHASE(I) + 360.0 * (RRATE(I) * DT
     *                     + 0.5 * RACCL(I) * DT**2)
                     ELSE
                        ISBD(I) = FBLANK
                        ISBDER(I) = FBLANK
                        IRT(I) = FBLANK
                        IRTERR(I) = FBLANK
                        IPHASE(I) = FBLANK
                     END IF
   40             CONTINUE
                  CALL OTABBS (BSTAB, 'WRIT', OUTREC, NUMIF,
     *               TIMES(TINDEX), INTERV, BASELN, SUBARR, STOKES,
     *               SOURCE, VAMP, SAMP, IMBD, IMBDER, MBDAMB, ISBD,
     *               ISBDER, SBDAMB, IRT, IRTERR, RTAMB, RACCL, ACCERR,
     *               IPHASE, PHSERR, IRET)
                  IF (IRET .NE. 0) GO TO 990
                  END IF
                  TINDEX = TINDEX + 1
                  GO TO 30
               END IF
            ELSE
               CALL OTABBS (BSTAB, 'WRIT', OUTREC, NUMIF, TIME, INTERV,
     *            BASELN, SUBARR, STOKES, SOURCE, VAMP, SAMP, RMBD,
     *            MBDERR,MBDAMB, RSBD, SBDERR, SBDAMB, RRATE, RTERR,
     *            RTAMB, RACCL,ACCERR, RPHASE, PHSERR, IRET)
               IF (IRET .NE. 0) GO TO 990
            ENDIF
         END IF
         GO TO 10
      END IF
C
C     Close the tables:
C
      CALL TABCLO (INTAB, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL TABCLO (BSTAB, IRET)
      IF (IRET .NE. 0) GO TO 990
C
 990  MSGTXT = 'ERROR RAISED IN GRID'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE RDTIMS (INPUTS, UVDATA, MAXTIM, TIMES, NTIMES, IRET)
C-----------------------------------------------------------------------
C   Read a list of discrete times from the CL table attached to UVDATA
C   with the version number specified by the GAINVER adverb.
C
C   Inputs:
C      INPUTS    C*(*)    INPUTS object used to access adverbs
C      UVDATA    C*(*)    UVDATA object used to access uv file
C      MAXTIM    I        Maximum number of times
C
C   Outputs:
C      TIMES     D(MAXTIM)    List of times in ascending order
C      NTIMES    I            Number of times in list
C      IRET      I            Error code (0 implies no errors detected)
C
C   Preconditions:
C      MAXTIM > 0
C      INPUTS and UVDATA are initialized
C
C   Postconditions:
C      if IRET = 0
C         1 <= NTIMES <= MAXTIM
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVDATA*(*)
      INTEGER   MAXTIM, NTIMES, IRET
      DOUBLE PRECISION TIMES(MAXTIM)
C
C  Local variables
C     CLTAB     TABLE object used to access CL table
C     SORT      CL table sort order
C     NROWS     Number of rows in CL table
C     ROW       Current row in CL table
C     TIME      CL record time-stamp
C     CLVER     Value of GAINVER adverb
C     TYPE      Attribute type code
C     DIM       Attribute dimensions
C
      CHARACTER CLTAB*9, CDUMMY
      INTEGER SORT(2), NROWS, ROW, TYPE, DIM(3), CLVER
      DOUBLE PRECISION TIME
      PARAMETER (CLTAB = 'CL table')
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Open the CL table and read the sort order
C
      CALL INGET (INPUTS, 'GAINVER', TYPE, DIM, CLVER, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL UV2TAB (UVDATA, CLTAB, 'CL', CLVER, IRET)
      IF (IRET .NE. 0) GO TO 990
      CALL TABOPN (CLTAB, 'READ', IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'CAN NOT OPEN INPUT CL TABLE'
         CALL MSGWRT (9)
         GO TO 990
      END IF
      CALL TABGET (CLTAB, 'SORT', TYPE, DIM, SORT, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
C
C     Sort the table if it is not in time order
C
      IF (SORT(1) .NE. 1) THEN
         CALL TABCLO (CLTAB, IRET)
         IF (IRET .NE. 0) GO TO 990
         CALL TBLSRT (CLTAB, 'TIME    ', 'TIME    ', IRET)
         IF (IRET .NE. 0) GO TO 990
         CALL TABOPN (CLTAB, 'READ', IRET)
         IF (IRET .NE. 0) GO TO 990
      END IF
C
C     Pass through the table, accumulating times
C
      NTIMES = 0
      CALL TABGET (CLTAB, 'NROW', TYPE, DIM, NROWS, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 990
      DO 10 ROW = 1, NROWS
         CALL TABDGT (CLTAB, ROW, 1, TYPE, DIM, TIME, CDUMMY, IRET)
         IF (IRET .GT. 0) GO TO 990
         IF (IRET .EQ. 0) THEN
            IF (NTIMES .EQ. 0) THEN
               NTIMES = NTIMES + 1
               TIMES(NTIMES) = TIME
            ELSE IF (TIME .GT. TIMES(NTIMES)) THEN
               NTIMES = NTIMES + 1
               IF (NTIMES .GT. MAXTIM) THEN
                  WRITE (MSGTXT, 1000) MAXTIM
                  CALL MSGWRT (9)
                  IRET = 1
                  GO TO 990
               ELSE
                  TIMES(NTIMES) = TIME
               END IF
            END IF
         END IF
   10 CONTINUE
C
      CALL TABCLO (CLTAB, IRET)
      IF (IRET .NE. 0) GO TO 990
C
 990  MSGTXT = 'ERROR RAISED IN RDTIMS'
      IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOO MANY TIMES (> ', I6, ') IN CL TABLE')
      END
C-----------------------------------------------------------------------
C   The following routines are taken from the public-domain LAPACK
C   library.
C-----------------------------------------------------------------------
      SUBROUTINE SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, INFO )
*
*  -- LAPACK driver routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, M, N, NRHS, RANK
      REAL               RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      REAL               A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SGELSX computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of matrices B and X. NRHS >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been overwritten by details of its
*          complete orthogonal factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) REAL array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, the N-by-NRHS solution matrix X.
*          If m >= n and RANK = n, the residual sum-of-squares for
*          the solution in the i-th column is given by the sum of
*          squares of elements N+1:M in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,M,N).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is an
*          initial column, otherwise it is a free column.  Before
*          the QR factorization of A, all initial columns are
*          permuted to the leading positions; only the remaining
*          free columns are moved as a result of column pivoting
*          during the factorization.
*          On exit, if JPVT(i) = k, then the i-th column of A*P
*          was the k-th column of A.
*
*  RCOND   (input) REAL
*          RCOND is used to determine the effective rank of A, which
*          is defined as the order of the largest leading triangular
*          submatrix R11 in the QR factorization with pivoting of A,
*          whose estimated condition number < 1/RCOND.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the order of the submatrix
*          R11.  This is the same as the order of the submatrix T11
*          in the complete orthogonal factorization of A.
*
*  WORK    (workspace) REAL array, dimension
*                      (max( min(M,N)+3*N, 2*min(M,N)+NRHS )),
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      REAL               ZERO, ONE, DONE, NTDONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0, DONE = ZERO,
     $                   NTDONE = ONE )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IASCL, IBSCL, ISMAX, ISMIN, J, K, MN
      REAL               ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, T1, T2
*     ..
*     .. External Functions ..
      REAL               SLAMCH, SLANGE
      EXTERNAL           SLAMCH, SLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEQPF, SLABAD, SLAIC1, SLASCL, SLASET, SLATZM,
     $                   SORM2R, STRSM, STZRQF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGELSX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      SMLNUM = SLAMCH( 'S' ) / SLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL SLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max elements outside range [SMLNUM,BIGNUM]
*
      ANRM = SLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL SLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL SLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL SLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 100
      END IF
*
      BNRM = SLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL SLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL SLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      CALL SGEQPF( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ), INFO )
*
*     workspace 3*N. Details of Householder rotations stored
*     in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL SLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 100
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         CALL SLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL SLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N )
     $   CALL STZRQF( RANK, N, A, LDA, WORK( MN+1 ), INFO )
*
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
      CALL SORM2R( 'Left', 'Transpose', M, NRHS, MN, A, LDA, WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), INFO )
*
*     workspace NRHS
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      CALL STRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
*
      DO 40 I = RANK + 1, N
         DO 30 J = 1, NRHS
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y' * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         DO 50 I = 1, RANK
            CALL SLATZM( 'Left', N-RANK+1, NRHS, A( I, RANK+1 ), LDA,
     $                   WORK( MN+I ), B( I, 1 ), B( RANK+1, 1 ), LDB,
     $                   WORK( 2*MN+1 ) )
   50    CONTINUE
      END IF
*
*     workspace NRHS
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 90 J = 1, NRHS
         DO 60 I = 1, N
            WORK( 2*MN+I ) = NTDONE
   60    CONTINUE
         DO 80 I = 1, N
            IF( WORK( 2*MN+I ).EQ.NTDONE ) THEN
               IF( JPVT( I ).NE.I ) THEN
                  K = I
                  T1 = B( K, J )
                  T2 = B( JPVT( K ), J )
   70             CONTINUE
                  B( JPVT( K ), J ) = T1
                  WORK( 2*MN+K ) = DONE
                  T1 = T2
                  K = JPVT( K )
                  T2 = B( JPVT( K ), J )
                  IF( JPVT( K ).NE.I )
     $               GO TO 70
                  B( I, J ) = T1
                  WORK( 2*MN+K ) = DONE
               END IF
            END IF
   80    CONTINUE
   90 CONTINUE
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL SLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL SLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL SLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL SLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL SLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL SLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
  100 CONTINUE
*
      RETURN
*
*     End of SGELSX
*
      END
      SUBROUTINE SLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            J, JOB
      REAL               C, GAMMA, S, SEST, SESTPR
*     ..
*     .. Array Arguments ..
      REAL               W( J ), X( J )
*     ..
*
*  Purpose
*  =======
*
*  SLAIC1 applies one step of incremental condition estimation in
*  its simplest version:
*
*  Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
*  lower triangular matrix L, such that
*           twonorm(L*x) = sest
*  Then SLAIC1 computes sestpr, s, c such that
*  the vector
*                  [ s*x ]
*           xhat = [  c  ]
*  is an approximate singular vector of
*                  [ L     0  ]
*           Lhat = [ w' gamma ]
*  in the sense that
*           twonorm(Lhat*xhat) = sestpr.
*
*  Depending on JOB, an estimate for the largest or smallest singular
*  value is computed.
*
*  Note that [s c]' and sestpr**2 is an eigenpair of the system
*
*      diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
*                                            [ gamma ]
*
*  where  alpha =  x'*w.
*
*  Arguments
*  =========
*
*  JOB     (input) INTEGER
*          = 1: an estimate for the largest singular value is computed.
*          = 2: an estimate for the smallest singular value is computed.
*
*  J       (input) INTEGER
*          Length of X and W
*
*  X       (input) REAL array, dimension (J)
*          The j-vector x.
*
*  SEST    (input) REAL
*          Estimated singular value of j by j matrix L
*
*  W       (input) REAL array, dimension (J)
*          The j-vector w.
*
*  GAMMA   (input) REAL
*          The diagonal element gamma.
*
*  SESTPR  (output) REAL
*          Estimated singular value of (j+1) by (j+1) matrix Lhat.
*
*  S       (output) REAL
*          Sine needed in forming xhat.
*
*  C       (output) REAL
*          Cosine needed in forming xhat.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0, TWO = 2.0E0 )
      REAL               HALF, FOUR
      PARAMETER          ( HALF = 0.5E0, FOUR = 4.0E0 )
*     ..
*     .. Local Scalars ..
      REAL               ABSALP, ABSEST, ABSGAM, ALPHA, B, COSINE, EPS,
     $                   NORMA, S1, S2, SINE, T, TEST, TMP, ZETA1, ZETA2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. External Functions ..
      REAL               SDOT, SLAMCH
      EXTERNAL           SDOT, SLAMCH
*     ..
*     .. Executable Statements ..
*
      EPS = SLAMCH( 'Epsilon' )
      ALPHA = SDOT( J, X, 1, W, 1 )
*
      ABSALP = ABS( ALPHA )
      ABSGAM = ABS( GAMMA )
      ABSEST = ABS( SEST )
*
      IF( JOB.EQ.1 ) THEN
*
*        Estimating largest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            S1 = MAX( ABSGAM, ABSALP )
            IF( S1.EQ.ZERO ) THEN
               S = ZERO
               C = ONE
               SESTPR = ZERO
            ELSE
               S = ALPHA / S1
               C = GAMMA / S1
               TMP = SQRT( S*S+C*C )
               S = S / TMP
               C = C / TMP
               SESTPR = S1*TMP
            END IF
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ONE
            C = ZERO
            TMP = MAX( ABSEST, ABSALP )
            S1 = ABSEST / TMP
            S2 = ABSALP / TMP
            SESTPR = TMP*SQRT( S1*S1+S2*S2 )
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ONE
               C = ZERO
               SESTPR = S2
            ELSE
               S = ZERO
               C = ONE
               SESTPR = S1
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               S = SQRT( ONE+TMP*TMP )
               SESTPR = S2*S
               C = ( GAMMA / S2 ) / S
               S = SIGN( ONE, ALPHA ) / S
            ELSE
               TMP = S2 / S1
               C = SQRT( ONE+TMP*TMP )
               SESTPR = S1*C
               S = ( ALPHA / S1 ) / C
               C = SIGN( ONE, GAMMA ) / C
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            B = ( ONE-ZETA1*ZETA1-ZETA2*ZETA2 )*HALF
            C = ZETA1*ZETA1
            IF( B.GT.ZERO ) THEN
               T = C / ( B+SQRT( B*B+C ) )
            ELSE
               T = SQRT( B*B+C ) - B
            END IF
*
            SINE = -ZETA1 / T
            COSINE = -ZETA2 / ( ONE+T )
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            SESTPR = SQRT( T+ONE )*ABSEST
            RETURN
         END IF
*
      ELSE IF( JOB.EQ.2 ) THEN
*
*        Estimating smallest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            SESTPR = ZERO
            IF( MAX( ABSGAM, ABSALP ).EQ.ZERO ) THEN
               SINE = ONE
               COSINE = ZERO
            ELSE
               SINE = -GAMMA
               COSINE = ALPHA
            END IF
            S1 = MAX( ABS( SINE ), ABS( COSINE ) )
            S = SINE / S1
            C = COSINE / S1
            TMP = SQRT( S*S+C*C )
            S = S / TMP
            C = C / TMP
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ZERO
            C = ONE
            SESTPR = ABSGAM
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ZERO
               C = ONE
               SESTPR = S1
            ELSE
               S = ONE
               C = ZERO
               SESTPR = S2
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               C = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST*( TMP / C )
               S = -( GAMMA / S2 ) / C
               C = SIGN( ONE, ALPHA ) / C
            ELSE
               TMP = S2 / S1
               S = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST / S
               C = ( ALPHA / S1 ) / S
               S = -SIGN( ONE, GAMMA ) / S
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            NORMA = MAX( ONE+ZETA1*ZETA1+ABS( ZETA1*ZETA2 ),
     $              ABS( ZETA1*ZETA2 )+ZETA2*ZETA2 )
*
*           See if root is closer to zero or to ONE
*
            TEST = ONE + TWO*( ZETA1-ZETA2 )*( ZETA1+ZETA2 )
            IF( TEST.GE.ZERO ) THEN
*
*              root is close to zero, compute directly
*
               B = ( ZETA1*ZETA1+ZETA2*ZETA2+ONE )*HALF
               C = ZETA2*ZETA2
               T = C / ( B+SQRT( ABS( B*B-C ) ) )
               SINE = ZETA1 / ( ONE-T )
               COSINE = -ZETA2 / T
               SESTPR = SQRT( T+FOUR*EPS*EPS*NORMA )*ABSEST
            ELSE
*
*              root is closer to ONE, shift by that amount
*
               B = ( ZETA2*ZETA2+ZETA1*ZETA1-ONE )*HALF
               C = ZETA1*ZETA1
               IF( B.GE.ZERO ) THEN
                  T = -C / ( B+SQRT( B*B+C ) )
               ELSE
                  T = B - SQRT( B*B+C )
               END IF
               SINE = -ZETA1 / T
               COSINE = -ZETA2 / ( ONE+T )
               SESTPR = SQRT( ONE+T+FOUR*EPS*EPS*NORMA )*ABSEST
            END IF
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            RETURN
*
         END IF
      END IF
      RETURN
*
*     End of SLAIC1
*
      END
      SUBROUTINE SLABAD( SMALL, LARGE )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      REAL               LARGE, SMALL
*     ..
*
*  Purpose
*  =======
*
*  SLABAD takes as input the values computed by SLAMCH for underflow and
*  overflow, and returns the square root of each of these values if the
*  log of LARGE is sufficiently large.  This subroutine is intended to
*  identify machines with a large exponent range, such as the Crays, and
*  redefine the underflow and overflow limits to be the square roots of
*  the values computed by SLAMCH.  This subroutine is needed because
*  SLAMCH does not compensate for poor arithmetic in the upper half of
*  the exponent range, as is found on a Cray.
*
*  Arguments
*  =========
*
*  SMALL   (input/output) REAL
*          On entry, the underflow threshold as computed by SLAMCH.
*          On exit, if LOG10(LARGE) is sufficiently large, the square
*          root of SMALL, otherwise unchanged.
*
*  LARGE   (input/output) REAL
*          On entry, the overflow threshold as computed by SLAMCH.
*          On exit, if LOG10(LARGE) is sufficiently large, the square
*          root of LARGE, otherwise unchanged.
*
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          LOG10, SQRT
*     ..
*     .. Executable Statements ..
*
*     If it looks like we're on a Cray, take the square root of
*     SMALL and LARGE to avoid overflow and underflow problems.
*
      IF( LOG10( LARGE ).GT.2000. ) THEN
         SMALL = SQRT( SMALL )
         LARGE = SQRT( LARGE )
      END IF
*
      RETURN
*
*     End of SLABAD
*
      END
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
      REAL             FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
*
*  Description
*  ===========
*
*  SLANGE returns the value
*
*     SLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in SLANGE as described
*          above.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.  When M = 0,
*          SLANGE is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*          SLANGE is set to zero.
*
*  A       (input) REAL array, dimension (LDA,N)
*          The m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) REAL array, dimension (LWORK),
*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      REAL               SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           SLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL SLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      SLANGE = VALUE
      RETURN
*
*     End of SLANGE
*
      END
      SUBROUTINE SLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      REAL               ALPHA, BETA
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  SLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be set.
*          = 'U':      Upper triangular part is set; the strictly lower
*                      triangular part of A is not changed.
*          = 'L':      Lower triangular part is set; the strictly upper
*                      triangular part of A is not changed.
*          Otherwise:  All of the matrix A is set.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  ALPHA   (input) REAL
*          The constant to which the offdiagonal elements are to be set.
*
*  BETA    (input) REAL
*          The constant to which the diagonal elements are to be set.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On exit, the leading m-by-n submatrix of A is set as follows:
*
*          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
*          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
*          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
*
*          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Set the strictly upper triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
*
*        Set the strictly lower triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
*
      ELSE
*
*        Set the leading m-by-n submatrix to ALPHA.
*
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
*
*     Set the first min(M,N) diagonal elements to BETA.
*
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
*
      RETURN
*
*     End of SLASET
*
      END
      SUBROUTINE SGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      REAL               A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SGEQPF computes a QR factorization with column pivoting of a
*  real M-by-N matrix A: A*P = Q*R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A. N >= 0
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the upper triangle of the array contains the
*          min(M,N)-by-N upper triangular matrix R; the elements
*          below the diagonal, together with the array TAU,
*          represent the orthogonal matrix Q as a product of
*          min(m,n) elementary reflectors.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*          to the front of A*P (a leading column); if JPVT(i) = 0,
*          the i-th column of A is a free column.
*          On exit, if JPVT(i) = k, then the i-th column of A*P
*          was the k-th column of A.
*
*  TAU     (output) REAL array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors.
*
*  WORK    (workspace) REAL array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(n)
*
*  Each H(i) has the form
*
*     H = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).
*
*  The matrix P is represented in jpvt as follows: If
*     jpvt(j) = i
*  then the jth column of P is the ith canonical unit vector.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ITEMP, J, MA, MN, PVT
      REAL               AII, TEMP, TEMP2
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEQR2, SLARF, SLARFG, SORM2R, SSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. External Functions ..
      INTEGER            ISAMAX
      REAL               SNRM2
      EXTERNAL           ISAMAX, SNRM2
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGEQPF', -INFO )
         RETURN
      END IF
*
      MN = MIN( M, N )
*
*     Move initial columns up front
*
      ITEMP = 1
      DO 10 I = 1, N
         IF( JPVT( I ).NE.0 ) THEN
            IF( I.NE.ITEMP ) THEN
               CALL SSWAP( M, A( 1, I ), 1, A( 1, ITEMP ), 1 )
               JPVT( I ) = JPVT( ITEMP )
               JPVT( ITEMP ) = I
            ELSE
               JPVT( I ) = I
            END IF
            ITEMP = ITEMP + 1
         ELSE
            JPVT( I ) = I
         END IF
   10 CONTINUE
      ITEMP = ITEMP - 1
*
*     Compute the QR factorization and update remaining columns
*
      IF( ITEMP.GT.0 ) THEN
         MA = MIN( ITEMP, M )
         CALL SGEQR2( M, MA, A, LDA, TAU, WORK, INFO )
         IF( MA.LT.N ) THEN
            CALL SORM2R( 'Left', 'Transpose', M, N-MA, MA, A, LDA, TAU,
     $                   A( 1, MA+1 ), LDA, WORK, INFO )
         END IF
      END IF
*
      IF( ITEMP.LT.MN ) THEN
*
*        Initialize partial column norms. The first n elements of
*        work store the exact column norms.
*
         DO 20 I = ITEMP + 1, N
            WORK( I ) = SNRM2( M-ITEMP, A( ITEMP+1, I ), 1 )
            WORK( N+I ) = WORK( I )
   20    CONTINUE
*
*        Compute factorization
*
         DO 40 I = ITEMP + 1, MN
*
*           Determine ith pivot column and swap if necessary
*
            PVT = ( I-1 ) + ISAMAX( N-I+1, WORK( I ), 1 )
*
            IF( PVT.NE.I ) THEN
               CALL SSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 )
               ITEMP = JPVT( PVT )
               JPVT( PVT ) = JPVT( I )
               JPVT( I ) = ITEMP
               WORK( PVT ) = WORK( I )
               WORK( N+PVT ) = WORK( N+I )
            END IF
*
*           Generate elementary reflector H(i)
*
            IF( I.LT.M ) THEN
               CALL SLARFG( M-I+1, A( I, I ), A( I+1, I ), 1, TAU( I ) )
            ELSE
               CALL SLARFG( 1, A( M, M ), A( M, M ), 1, TAU( M ) )
            END IF
*
            IF( I.LT.N ) THEN
*
*              Apply H(i) to A(i:m,i+1:n) from the left
*
               AII = A( I, I )
               A( I, I ) = ONE
               CALL SLARF( 'LEFT', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                     A( I, I+1 ), LDA, WORK( 2*N+1 ) )
               A( I, I ) = AII
            END IF
*
*           Update partial column norms
*
            DO 30 J = I + 1, N
               IF( WORK( J ).NE.ZERO ) THEN
                  TEMP = ONE - ( ABS( A( I, J ) ) / WORK( J ) )**2
                  TEMP = MAX( TEMP, ZERO )
                  TEMP2 = ONE + 0.05*TEMP*( WORK( J ) / WORK( N+J ) )**2
                  IF( TEMP2.EQ.ONE ) THEN
                     IF( M-I.GT.0 ) THEN
                        WORK( J ) = SNRM2( M-I, A( I+1, J ), 1 )
                        WORK( N+J ) = WORK( J )
                     ELSE
                        WORK( J ) = ZERO
                        WORK( N+J ) = ZERO
                     END IF
                  ELSE
                     WORK( J ) = WORK( J )*SQRT( TEMP )
                  END IF
               END IF
   30       CONTINUE
*
   40    CONTINUE
      END IF
      RETURN
*
*     End of SGEQPF
*
      END
      SUBROUTINE STRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  STRSM  solves one of the matrix equations
*
*     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*
*  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  The matrix X is overwritten on B.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry, SIDE specifies whether op( A ) appears on the left
*           or right of X as follows:
*
*              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*
*              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - REAL             array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain  the  right-hand  side  matrix  B,  and  on exit  is
*           overwritten by the solution matrix  X.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      REAL               TEMP
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRSM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*inv( A )*B.
*
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*inv( A' )*B.
*
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*inv( A ).
*
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*inv( A' ).
*
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of STRSM .
*
      END
      SUBROUTINE SLATZM( SIDE, M, N, V, INCV, TAU, C1, C2, LDC, WORK )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      REAL               TAU
*     ..
*     .. Array Arguments ..
      REAL               C1( LDC, * ), C2( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SLATZM applies a Householder matrix generated by STZRQF to a matrix.
*
*  Let P = I - tau*u*u',   u = ( 1 ),
*                              ( v )
*  where v is an (m-1) vector if SIDE = 'L', or a (n-1) vector if
*  SIDE = 'R'.
*
*  If SIDE equals 'L', let
*         C = [ C1 ] 1
*             [ C2 ] m-1
*               n
*  Then C is overwritten by P*C.
*
*  If SIDE equals 'R', let
*         C = [ C1, C2 ] m
*                1  n-1
*  Then C is overwritten by C*P.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form P * C
*          = 'R': form C * P
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) REAL array, dimension
*                  (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of P. V is not used
*          if TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0
*
*  TAU     (input) REAL
*          The value tau in the representation of P.
*
*  C1      (input/output) REAL array, dimension
*                         (LDC,N) if SIDE = 'L'
*                         (M,1)   if SIDE = 'R'
*          On entry, the n-vector C1 if SIDE = 'L', or the m-vector C1
*          if SIDE = 'R'.
*
*          On exit, the first row of P*C if SIDE = 'L', or the first
*          column of C*P if SIDE = 'R'.
*
*  C2      (input/output) REAL array, dimension
*                         (LDC, N)   if SIDE = 'L'
*                         (LDC, N-1) if SIDE = 'R'
*          On entry, the (m - 1) x n matrix C2 if SIDE = 'L', or the
*          m x (n - 1) matrix C2 if SIDE = 'R'.
*
*          On exit, rows 2:m of P*C if SIDE = 'L', or columns 2:m of C*P
*          if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the arrays C1 and C2. LDC >= (1,M).
*
*  WORK    (workspace) REAL array, dimension
*                      (N) if SIDE = 'L'
*                      (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           SAXPY, SCOPY, SGEMV, SGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( ( MIN( M, N ).EQ.0 ) .OR. ( TAU.EQ.ZERO ) )
     $   RETURN
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        w := C1 + v' * C2
*
         CALL SCOPY( N, C1, LDC, WORK, 1 )
         CALL SGEMV( 'Transpose', M-1, N, ONE, C2, LDC, V, INCV, ONE,
     $               WORK, 1 )
*
*        [ C1 ] := [ C1 ] - tau* [ 1 ] * w'
*        [ C2 ]    [ C2 ]        [ v ]
*
         CALL SAXPY( N, -TAU, WORK, 1, C1, LDC )
         CALL SGER( M-1, N, -TAU, V, INCV, WORK, 1, C2, LDC )
*
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*        w := C1 + C2 * v
*
         CALL SCOPY( M, C1, 1, WORK, 1 )
         CALL SGEMV( 'No transpose', M, N-1, ONE, C2, LDC, V, INCV, ONE,
     $               WORK, 1 )
*
*        [ C1, C2 ] := [ C1, C2 ] - tau* w * [ 1 , v']
*
         CALL SAXPY( M, -TAU, WORK, 1, C1, 1 )
         CALL SGER( M, N-1, -TAU, WORK, 1, V, INCV, C2, LDC )
      END IF
*
      RETURN
*
*     End of SLATZM
*
      END
      SUBROUTINE STZRQF( M, N, A, LDA, TAU, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * )
*     ..
*
*  Purpose
*  =======
*
*  STZRQF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
*  to upper triangular form by means of orthogonal transformations.
*
*  The upper trapezoidal matrix A is factored as
*
*     A = ( R  0 ) * Z,
*
*  where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
*  triangular matrix.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= M.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the leading M-by-N upper trapezoidal part of the
*          array A must contain the matrix to be factorized.
*          On exit, the leading M-by-M upper triangular part of A
*          contains the upper triangular matrix R, and elements M+1 to
*          N of the first M rows of A, with the array TAU, represent the
*          orthogonal matrix Z as a product of M elementary reflectors.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) REAL array, dimension (M)
*          The scalar factors of the elementary reflectors.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The factorization is obtained by Householder's method.  The kth
*  transformation matrix, Z( k ), which is used to introduce zeros into
*  the ( m - k + 1 )th row of A, is given in the form
*
*     Z( k ) = ( I     0   ),
*              ( 0  T( k ) )
*
*  where
*
*     T( k ) = I - tau*u( k )*u( k )',   u( k ) = (   1    ),
*                                                 (   0    )
*                                                 ( z( k ) )
*
*  tau is a scalar and z( k ) is an ( n - m ) element vector.
*  tau and z( k ) are chosen to annihilate the elements of the kth row
*  of X.
*
*  The scalar tau is returned in the kth element of TAU and the vector
*  u( k ) in the kth row of A, such that the elements of z( k ) are
*  in  a( k, m + 1 ), ..., a( k, n ). The elements of R are returned in
*  the upper triangular part of A.
*
*  Z is given by
*
*     Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K, M1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           SAXPY, SCOPY, SGEMV, SGER, SLARFG, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'STZRQF', -INFO )
         RETURN
      END IF
*
*     Perform the factorization.
*
      IF( M.EQ.0 )
     $   RETURN
      IF( M.EQ.N ) THEN
         DO 10 I = 1, N
            TAU( I ) = ZERO
   10    CONTINUE
      ELSE
         M1 = MIN( M+1, N )
         DO 20 K = M, 1, -1
*
*           Use a Householder reflection to zero the kth row of A.
*           First set up the reflection.
*
            CALL SLARFG( N-M+1, A( K, K ), A( K, M1 ), LDA, TAU( K ) )
*
            IF( ( TAU( K ).NE.ZERO ) .AND. ( K.GT.1 ) ) THEN
*
*              We now perform the operation  A := A*P( k ).
*
*              Use the first ( k - 1 ) elements of TAU to store  a( k ),
*              where  a( k ) consists of the first ( k - 1 ) elements of
*              the  kth column  of  A.  Also  let  B  denote  the  first
*              ( k - 1 ) rows of the last ( n - m ) columns of A.
*
               CALL SCOPY( K-1, A( 1, K ), 1, TAU, 1 )
*
*              Form   w = a( k ) + B*z( k )  in TAU.
*
               CALL SGEMV( 'No transpose', K-1, N-M, ONE, A( 1, M1 ),
     $                     LDA, A( K, M1 ), LDA, ONE, TAU, 1 )
*
*              Now form  a( k ) := a( k ) - tau*w
*              and       B      := B      - tau*w*z( k )'.
*
               CALL SAXPY( K-1, -TAU( K ), TAU, 1, A( 1, K ), 1 )
               CALL SGER( K-1, N-M, -TAU( K ), TAU, 1, A( K, M1 ), LDA,
     $                    A( 1, M1 ), LDA )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of STZRQF
*
      END
      SUBROUTINE SORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SORM2R overwrites the general real m by n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'T', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'T',
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by SGEQRF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'T': apply Q' (Transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) REAL array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          SGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) REAL array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by SGEQRF.
*
*  C       (input/output) REAL array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) REAL array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      REAL               AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           SLARF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORM2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i)
*
         AII = A( I, I )
         A( I, I ) = ONE
         CALL SLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of SORM2R
*
      END
      SUBROUTINE SLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      REAL               CFROM, CTO
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  SLASCL multiplies the M by N real matrix A by the real scalar
*  CTO/CFROM.  This is done without over/underflow as long as the final
*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
*  A may be full, upper triangular, lower triangular, upper Hessenberg,
*  or banded.
*
*  Arguments
*  =========
*
*  TYPE    (input) CHARACTER*1
*          TYPE indices the storage type of the input matrix.
*          = 'G':  A is a full matrix.
*          = 'L':  A is a lower triangular matrix.
*          = 'U':  A is an upper triangular matrix.
*          = 'H':  A is an upper Hessenberg matrix.
*          = 'B':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the lower
*                  half stored.
*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the upper
*                  half stored.
*          = 'Z':  A is a band matrix with lower bandwidth KL and upper
*                  bandwidth KU.
*
*  KL      (input) INTEGER
*          The lower bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  KU      (input) INTEGER
*          The upper bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  CFROM   (input) REAL
*  CTO     (input) REAL
*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
*          without over/underflow if the final result CTO*A(I,J)/CFROM
*          can be represented without over/underflow.  CFROM must be
*          nonzero.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,M)
*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
*          storage type.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  INFO    (output) INTEGER
*          0  - successful exit
*          <0 - if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      REAL               BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH
      EXTERNAL           LSAME, SLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
*
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
*
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLASCL', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     Get machine parameters
*
      SMLNUM = SLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
*
      CFROMC = CFROM
      CTOC = CTO
*
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
*
      IF( ITYPE.EQ.0 ) THEN
*
*        Full matrix
*
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
*
      ELSE IF( ITYPE.EQ.1 ) THEN
*
*        Lower triangular matrix
*
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
*
      ELSE IF( ITYPE.EQ.2 ) THEN
*
*        Upper triangular matrix
*
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
*
      ELSE IF( ITYPE.EQ.3 ) THEN
*
*        Upper Hessenberg matrix
*
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
*
      ELSE IF( ITYPE.EQ.4 ) THEN
*
*        Lower half of a symmetric band matrix
*
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
*
      ELSE IF( ITYPE.EQ.5 ) THEN
*
*        Upper half of a symmetric band matrix
*
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
*
      ELSE IF( ITYPE.EQ.6 ) THEN
*
*        Band matrix
*
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
*
      END IF
*
      IF( .NOT.DONE )
     $   GO TO 10
*
      RETURN
*
*     End of SLASCL
*
      END
      INTEGER FUNCTION ISAMAX(N,SX,INCX)
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SMAX
      INTEGER I,INCX,IX,N
C
      ISAMAX = 0
      IF( N.LT.1 .OR. INCX.LE.0 ) RETURN
      ISAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMAX = ABS(SX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(ABS(SX(IX)).LE.SMAX) GO TO 5
         ISAMAX = I
         SMAX = ABS(SX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         IF(ABS(SX(I)).LE.SMAX) GO TO 30
         ISAMAX = I
         SMAX = ABS(SX(I))
   30 CONTINUE
      RETURN
      END
      SUBROUTINE SGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - REAL             array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - REAL             array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGER  ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of SGER  .
*
      END
      SUBROUTINE SGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SGEQR2 computes a QR factorization of a real m by n matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(m,n) by n upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) REAL array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) REAL array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      REAL               AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           SLARF, SLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGEQR2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
         CALL SLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            AII = A( I, I )
            A( I, I ) = ONE
            CALL SLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
*
*     End of SGEQR2
*
      END
      SUBROUTINE SGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - REAL            .
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - REAL             array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - REAL             array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - REAL            .
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - REAL             array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of SGEMV .
*
      END
      SUBROUTINE SLARFG( N, ALPHA, X, INCX, TAU )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      REAL               ALPHA, TAU
*     ..
*     .. Array Arguments ..
      REAL               X( * )
*     ..
*
*  Purpose
*  =======
*
*  SLARFG generates a real elementary reflector H of order n, such
*  that
*
*        H * ( alpha ) = ( beta ),   H' * H = I.
*            (   x   )   (   0  )
*
*  where alpha and beta are scalars, and x is an (n-1)-element real
*  vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a real scalar and v is a real (n-1)-element
*  vector.
*
*  If the elements of x are all zero, then tau = 0 and H is taken to be
*  the unit matrix.
*
*  Otherwise  1 <= tau <= 2.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) REAL
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) REAL array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) REAL
*          The value tau.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J, KNT
      REAL               BETA, RSAFMN, SAFMIN, XNORM
*     ..
*     .. External Functions ..
      REAL               SLAMCH, SLAPY2, SNRM2
      EXTERNAL           SLAMCH, SLAPY2, SNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           SSCAL
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
*
      XNORM = SNRM2( N-1, X, INCX )
*
      IF( XNORM.EQ.ZERO ) THEN
*
*        H  =  I
*
         TAU = ZERO
      ELSE
*
*        general case
*
         BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = SLAMCH( 'S' ) / SLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
*
*           XNORM, BETA may be inaccurate; scale X and recompute them
*
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL SSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
*
*           New BETA is at most 1, at least SAFMIN
*
            XNORM = SNRM2( N-1, X, INCX )
            BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
*
*           If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
*
      RETURN
*
*     End of SLARFG
*
      END
      REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*),STEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      STEMP = 0.0E0
      SDOT = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        STEMP = STEMP + SX(IX)*SY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      SDOT = STEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        STEMP = STEMP + SX(I)*SY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        STEMP = STEMP + SX(I)*SY(I) + SX(I + 1)*SY(I + 1) +
     *   SX(I + 2)*SY(I + 2) + SX(I + 3)*SY(I + 3) + SX(I + 4)*SY(I + 4)
   50 CONTINUE
   60 SDOT = STEMP
      RETURN
      END
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*),SA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (SA .EQ. 0.0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SY(I) + SA*SX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        SY(I) = SY(I) + SA*SX(I)
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SSWAP (N,SX,INCX,SY,INCY)
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*),STEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        STEMP = SX(IX)
        SX(IX) = SY(IY)
        SY(IY) = STEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
        STEMP = SX(I + 1)
        SX(I + 1) = SY(I + 1)
        SY(I + 1) = STEMP
        STEMP = SX(I + 2)
        SX(I + 2) = SY(I + 2)
        SY(I + 2) = STEMP
   50 CONTINUE
      RETURN
      END
      REAL             FUNCTION SNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      REAL                              X( * )
*     ..
*
*  SNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     SNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to SLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      REAL                  ONE         , ZERO
      PARAMETER           ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     .. Local Scalars ..
      INTEGER               IX
      REAL                  ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL SLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      SNRM2 = NORM
      RETURN
*
*     End of SNRM2.
*
      END
      SUBROUTINE SLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      REAL               TAU
*     ..
*     .. Array Arguments ..
      REAL               C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  SLARF applies a real elementary reflector H to a real m by n matrix
*  C, from either the left or the right. H is represented in the form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) REAL array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) REAL
*          The value tau in the representation of H.
*
*  C       (input/output) REAL array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) REAL array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEMV, SGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C' * v
*
            CALL SGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO,
     $                  WORK, 1 )
*
*           C := C - v * w'
*
            CALL SGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C * v
*
            CALL SGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
*
*           C := C - w * v'
*
            CALL SGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
*
*     End of SLARF
*
      END
      SUBROUTINE SCOPY(N,SX,INCX,SY,INCY)
C
C     COPIES A VECTOR, X, TO A VECTOR, Y.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        SY(I) = SX(I)
        SY(I + 1) = SX(I + 1)
        SY(I + 2) = SX(I + 2)
        SY(I + 3) = SX(I + 3)
        SY(I + 4) = SX(I + 4)
        SY(I + 5) = SX(I + 5)
        SY(I + 6) = SX(I + 6)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SLASSQ( N, X, INCX, SCALE, SUMSQ )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      REAL               SCALE, SUMSQ
*     ..
*     .. Array Arguments ..
      REAL               X( * )
*     ..
*
*  Purpose
*  =======
*
*  SLASSQ  returns the values  scl  and  smsq  such that
*
*     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
*
*  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
*  assumed to be non-negative and  scl  returns the value
*
*     scl = max( scale, abs( x( i ) ) ).
*
*  scale and sumsq must be supplied in SCALE and SUMSQ and
*  scl and smsq are overwritten on SCALE and SUMSQ respectively.
*
*  The routine makes only one pass through the vector x.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements to be used from the vector X.
*
*  X       (input) REAL
*          The vector for which a scaled sum of squares is computed.
*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector X.
*          INCX > 0.
*
*  SCALE   (input/output) REAL
*          On entry, the value  scale  in the equation above.
*          On exit, SCALE is overwritten with  scl , the scaling factor
*          for the sum of squares.
*
*  SUMSQ   (input/output) REAL
*          On entry, the value  sumsq  in the equation above.
*          On exit, SUMSQ is overwritten with  smsq , the basic sum of
*          squares from which  scl  has been factored out.
*
* =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IX
      REAL               ABSXI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( X( IX ).NE.ZERO ) THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
*
*     End of SLASSQ
*
      END
      REAL             FUNCTION SLAPY2( X, Y )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      REAL               X, Y
*     ..
*
*  Purpose
*  =======
*
*  SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
*
*  Arguments
*  =========
*
*  X       (input) REAL
*  Y       (input) REAL
*          X and Y specify the values x and y.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      REAL               W, XABS, YABS, Z
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         SLAPY2 = W
      ELSE
         SLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
*
*     End of SLAPY2
*
      END
      SUBROUTINE SSCAL(N,SA,SX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SA,SX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        SX(I) = SA*SX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END
