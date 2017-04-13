      SUBROUTINE AVERT (TB, TE, IBLCOD, NBL, BUFFER, TAU, TAUMIN,
     *   TAUMAX, IRET)
C-----------------------------------------------------------------------
C! Search a uv-file to determine integration times on each baseline
C# UV IO-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2001, 2011, 2013, 2015
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
C   Search a uv-file to determine integration times on each baseline
C   Uses UVGET which must have all essential parameters set in DSEL.INC
C   This uses FG, NX etc to access desired data quickly.
C   Uses UVGET so adverbs must be set - will temporarily use SUBARR=1
C   if needed.
C   Inputs:
C      TB       R      The beginning of selected time interval (days)
C      TE       R      The end of selected time interval (days)
C      IBLCOD   I(*)   Desired baselines (32768 * ant_1 + ant_2)
C      NBL      I      Element count in IBLCOD.
C   Input/output:
C      BUFFER   R(*)   I/O buffer for uv-file access
C   Output:
C      TAU      R(*)   Array of pre-average times (sec). Same
C                         order as IBLCOD.
C      TAUMIN   R      Minimum integration time found (sec).
C      TAUMAX   R      Maximum integration time found (sec).
C      IRET     I      Error code: -1 => no int time parameter
C                         0 okay, > 0 I/O error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL      TB, TE, BUFFER(*), TAU(*), TAUMIN, TAUMAX
      INTEGER   IBLCOD(*), NBL, IRET
C
      LOGICAL   WFOUND
      REAL      TIMCUR, TINT, RPARM(20)
      INTEGER   JBL, I, IBL, IERR, LSUB, NCOUNT, IANT1, IANT2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
C----------------------------------------------------------------------
C                                        Initialization
      CALL RFILL (MXBASE, 0.0, TAU)
      TAUMIN = 1.0E20
      TAUMAX = 0.0
C                                        Exit if no integration time
C                                        random parameter.
      IRET = -1
      IF (ILOCIT.LT.0) GO TO 999
      NCOUNT = 0
C
C                                       Init vis file for read
      LSUB = SUBARR
      IF (SUBARR.EQ.0) SUBARR = 1
      CALL UVGET ('INIT', RPARM, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT DATA FOR READ'
         GO TO 990
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'NO DATA FOUND: CHECK ADVERB VALUES'
         IRET = 10
         GO TO 990
         END IF
C
      WFOUND = .FALSE.
C                                       Loop, reading uv_records
 100  CALL UVGET ('READ', RPARM, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING UV DATA'
         GO TO 990
      ELSE IF ((IRET.LT.0) .AND. (NCOUNT.LE.0)) THEN
         MSGTXT = 'NO DATA FOUND: CHECK ADVERB VALUES'
         IRET = 10
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
         NCOUNT = NCOUNT + 1
C                                       Extract time, baseline,
C                                       integration time and subarray
         TIMCUR = RPARM(1+ILOCT)
         IF (ILOCB.GE.0) THEN
            JBL = RPARM(1+ILOCB) + 0.1
            IANT1 = JBL / 256
            IANT2 = JBL - 256*IANT1
         ELSE
            IANT1 = RPARM(1+ILOCA1) + 0.1
            IANT2 = RPARM(1+ILOCA2) + 0.1
            END IF
         JBL = 32768 * IANT1 + IANT2
         TINT = RPARM(1+ILOCIT)
C                                       Check time range (assume
C                                       TB sort order) and subarray
         IF ((TIMCUR.GE.TB) .AND. (TIMCUR.LE.TE)) THEN
C                                       Match baseline code.
            IBL = 0
            DO 120 I = 1,NBL
               IF (JBL.EQ.IBLCOD(I)) IBL = I
120            CONTINUE
C                                       Update pre-average times
            IF (IBL.NE.0) THEN
C                                       Assume constant in time.
               IF (TAU(IBL).EQ.0.0) THEN
                  TAU(IBL) = TINT
                  TAUMIN = MIN (TAUMIN, TINT)
                  TAUMAX = MAX (TAUMAX, TINT)
C                                       All baselines found ?
                  WFOUND = .TRUE.
                  DO 140 I = 1,NBL
                     IF (TAU(I).EQ.0.0) WFOUND = .FALSE.
140                  CONTINUE
                  END IF
               END IF
            END IF
C                                       Read next record
         IF ((TIMCUR.LT.TE) .AND. (.NOT.WFOUND)) GO TO 100
         END IF
      GO TO 995
C                                        Error
 990  CALL MSGWRT (8)
C                                       Close uv file.
 995  CALL UVGET ('CLOS', RPARM, BUFFER, IERR)
      SUBARR = LSUB
      IRET = MAX (0, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVERT: ERROR',I4,' ON ',A)
      END
