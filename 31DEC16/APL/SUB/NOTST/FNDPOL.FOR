       SUBROUTINE FNDPOL (STKCHR, IERR)
C-----------------------------------------------------------------------
C! Given Stokes parameter, checks whether compatible with data or not.
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2007, 2010-2011
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
C   Routine to check that a spcified Stokes' parameter is consistent
C   with the data.  This routine is intended to be used with the
C   calibration package (UVGET/DGINIT).  In particular, possible
C   invalid  warning messages may be issued for Stokes' Q or U if the
C   initialization call to UVGET has not been made (or at least the
C   value of DOPOL set).
C   In the case of STKCHR = ' ', the routine returns the actual Stokes
C   in the data file.
C   Currently, there are 19 valid Stokes codes (including '    ').
C   This routine added so that UVGET will not have problems handling,
C   for example, 'LL' only.
C   Inputs:
C      STKCHR   C*4   Requested Stokes parameter:
C                     'I', 'Q', 'U', 'V', 'IQU', 'IQUV', 'IV', 'QU',
C                     'RR', 'LL', 'RL', 'LR', 'RRLL', 'RLLR', 'RLRL'
C                     'XX', 'YY', 'XY', 'YX', 'XXYY', 'XYYX', 'XYXY'
C                     'HALF', 'CROPS', 'FULL'
C                     Or blank => FNDPOL returns a STKCHR of its own
C   Input in common (DSEL.INC):
C      DOPOL     I       >0 if polarization calibration requested.
C      CATUV     I(256)  Catalog header for data.
C   Outputs:
C      IERR      I    Return code, 0=>OK else data incompatible
C   Note that this routine limits the data in ways that should not be
C   necessary ig DGINIT were smarter.
C-----------------------------------------------------------------------
      CHARACTER STKCHR*4
      INTEGER   IERR
C
      INTEGER   NOMODE, NOMODF
      PARAMETER (NOMODE = 22,  NOMODF = 7)
      HOLLERITH CATUH(256)
      DOUBLE PRECISION CATUD(128)
      REAL      CATUR(256)
      LOGICAL   IQUV, RRLL, XXYY, SINGLE, FORMAL
      CHARACTER POLMOD(NOMODE)*4, FORMOD(NOMODF)*4, LSTOKE*4
      INTEGER   STKDIM, STKREF, POLCOD, KLOCS, I, STKINC, IROUND,
     *   STKMCH, STKMAX
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATUV, CATUD, CATUR, CATUH)
      DATA POLMOD /'I', 'Q', 'U', 'V', 'IQU', 'IQUV', 'IV', 'QU',
     *   'RR', 'LL', 'RL', 'LR', 'RRLL', 'RLLR', 'RLRL',
     *   'XX', 'YY', 'XY', 'YX', 'XXYY', 'XYYX', 'XYXY'/
      DATA FORMOD /'F','Q','U','V','FQU','FQUV','FV'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a single source file
C                                       (i.e. a "SOURCE" random
C                                       parameter).
      CALL AXEFND (8, 'SOURCE  ', CATUV(KIPCN), CATUH(KHPTP), KLOCS,
     *   IERR)
      CALL MULSDB (CATUV, SINGLE)
      SINGLE = .NOT.SINGLE
C                                      Find Stokes info in the header.
      CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATUH(KHCTP), KLOCS,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C
      IF (CATUD(KDCRV+KLOCS) .GT. 0.0D0) STKREF = CATUD(KDCRV+KLOCS)
     *   + 0.5D0
      IF (CATUD(KDCRV+KLOCS) .LT. 0.0D0) STKREF = CATUD(KDCRV+KLOCS)
     *   - 0.5D0
      STKDIM = CATUV(KINAX+KLOCS)
      STKINC = IROUND (CATUR(KRCIC+KLOCS))
      STKMAX = STKREF + (STKDIM-1)*STKINC
C                                      Obvious checks.
      IF ((STKDIM.LT.1) .OR. (STKDIM.GT.4)) THEN
         WRITE (MSGTXT,1001) STKDIM
         IERR = 4
         GO TO 990
         END IF
C                                       increment sign
      IF ((STKREF*STKINC.LE.0) .AND. (STKDIM.GT.1)) THEN
         WRITE (MSGTXT,1002) STKINC
         IERR = 4
         GO TO 990
         END IF
C
      IF ((STKREF.EQ.0) .OR. (STKREF.GT.4) .OR. (STKREF.LT.-8)) THEN
         MSGTXT = 'FNDPOL: ILLEGAL REFERENCE PIXEL VALUE ON STOKES AXIS'
         IERR = 6
         GO TO 990
         END IF
C
      IQUV = (STKREF.GE.1)  .AND. (STKREF.LE.4)  .AND.
     *   (STKMAX.GE.1) .AND. (STKMAX.LE.4)
      RRLL = (STKREF.LE.-1) .AND. (STKREF.GE.-4) .AND.
     *   (STKMAX.LE.-1) .AND. (STKMAX.GE.-4)
      XXYY = (STKREF.LE.-5) .AND. (STKREF.GE.-8) .AND.
     *   (STKMAX.LE.-5) .AND. (STKMAX.GE.-8)
      IF ((.NOT.IQUV) .AND. (.NOT.RRLL) .AND. (.NOT.XXYY)) THEN
         MSGTXT = 'FNDPOL: ILLEGAL COORDINATES ON STOKES AXIS'
         IERR = 6
         GO TO 990
         END IF
C                                       select output stokes
C                                       limits of DGINIT are honored
      IERR = 0
      IF (STKCHR.EQ.' ') THEN
C                                       (A) True Stokes.
C
         IF (IQUV) THEN
            MSGTXT = 'FNDPOL: ILLEGAL COMBINATION OF I,Q,U & V IN DATA'
C                                       I or Q or U or V
            IF (STKDIM.EQ.1) THEN
               STKCHR = POLMOD(STKREF)
C                                       IV
            ELSE IF (STKDIM.EQ.2) THEN
               IERR = 1
C                                        Can't cope with non-standard
C                                        combinations; e.g. QU only
            ELSE
               IF (STKREF.NE.1) THEN
                  IERR = 1
C                                        IQU, IQUV
               ELSE
                  IF (STKDIM.EQ.3) THEN
                     STKCHR = 'IQU '
                  ELSE
                     STKCHR = 'IQUV'
                     END IF
                  END IF
               END IF
C                                      (B) R, L combinations.
C                                       Must have STokes increment = -1
C                                       unless we have a single pol'n
         ELSE IF (RRLL) THEN
            IF ((STKDIM.NE.1) .AND. (STKINC.NE.-1)) THEN
               MSGTXT = 'FNDPOL: ILLEGAL STOKES AXIS INCREMENT'
               IERR = 1
               GO TO 990
               END IF
C                                        Test each case in the
            MSGTXT = 'FNDPOL: ILLEGAL COMBINATION OF RR,LL,RL,LR'
     *         // ' IN DATA'
C                                        POLMOD list
            IF (STKREF.EQ.-1) THEN
C                                        RR only.
               IF (STKDIM.EQ.1) THEN
                  STKCHR = 'RR  '
C                                        RR, LL
               ELSE IF (STKDIM.EQ.2) THEN
                  STKCHR = 'RRLL'
C                                        RR, LL, RL and LR
               ELSE IF (STKDIM.EQ.4) THEN
                  STKCHR = 'FULL'
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: THE DATA CONTAIN ONLY RR,LL,RL: '//
     *               'AN ILLEGAL COMBINATION'
                  END IF
C                                        no RR
            ELSE IF (STKREF.EQ.-2) THEN
C                                       LL only
               IF (STKDIM.EQ.1) THEN
                  STKCHR = 'LL  '
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: THE DATA CONTAIN ONLY LL,RL,(LR): '
     *               // 'AN ILLEGAL COMBINATION'
                  END IF
C                                       RL and/or LR
            ELSE IF ((STKREF.EQ.-3) .OR. (STKREF.EQ.-4)) THEN
C                                        RL or LR only
               IF (STKDIM.EQ.1) THEN
                  IF (STKREF.EQ.-3) STKCHR = 'RL  '
                  IF (STKREF.EQ.-4) STKCHR = 'LR  '
               ELSE IF ((STKDIM.EQ.2) .AND. (STKREF.EQ.-3)) THEN
                  STKCHR = 'RLLR'
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: SPECIFY WHICH OF RL AND LR' //
     *               ' IS NEEDED'
                  END IF
               END IF
C                                      (C) XX,YY,XY,YX combinations.
C                                       Must have STokes increment = -1
C                                       unless we have a single pol'n
         ELSE IF (XXYY) THEN
            IF ((STKDIM.NE.1) .AND. (STKINC.NE.-1)) THEN
               MSGTXT = 'FNDPOL: ILLEGAL STOKES AXIS INCREMENT'
               IERR = 1
               GO TO 990
               END IF
            MSGTXT = 'FNDPOL: ILLEGAL COMBINATION OF XX,YY,XY,YX'
     *         // ' IN DATA'
C                                        Test all the POLMOD cases
            IF (STKREF.EQ.-5) THEN
C                                        XX only.
               IF (STKDIM.EQ.1) THEN
                  STKCHR = 'XX  '
C                                        XX, YY
               ELSE IF (STKDIM.EQ.2) THEN
                  STKCHR = 'XXYY'
C                                        XX,YY,XY,YX
               ELSE IF (STKDIM.EQ.4) THEN
                  STKCHR = 'FULL'
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: THE DATA CONTAIN ONLY XX,YY,XY: '//
     *               'AN ILLEGAL COMBINATION'
                  END IF
            ELSE IF (STKREF.EQ.-6) THEN
C                                        YY only
               IF (STKDIM.EQ.1) THEN
                  STKCHR = 'YY  '
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: THE DATA CONTAIN ONLY YY,XY,(YX): '
     *               // 'AN ILLEGAL COMBINATION'
                  END IF
            ELSE IF ((STKREF.EQ.-7) .OR. (STKREF.EQ.-8)) THEN
C                                        XY or YX only
               IF (STKDIM.EQ.1) THEN
                  IF (STKREF.EQ.-7) STKCHR = 'XY  '
                  IF (STKREF.EQ.-8) STKCHR = 'YX  '
               ELSE IF ((STKDIM.EQ.2) .AND. (STKREF.EQ.-7)) THEN
                  STKCHR = 'XYYX'
               ELSE
                  IERR = 1
                  MSGTXT = 'FNDPOL: SPECIFY WHICH OF XY AND YX' //
     *               ' IS NEEDED'
                  END IF
               END IF
            END IF
         GO TO 990
C=======================================================================
C  In the following section, STKCHR has already been set.  Thus, only
C  make sure that the requested STKCHR is available. In the previous
C  section, we also had to set STKCHR as it was blank on input, and
C  STKCHR had to be set from the list of 19 available codes.  This
C  means some combinations of data (not requests, but what is actually
C  in the data, regardless of how it got to be like that) such as QUV,
C  or RR,LL,RL were illegal.I think this is an unneccessary restriction
C  and I do not trap these "illegal" combinations below.  If its there
C  and they want it, give it to them.  DGINIT should fail if it can't
C  provide what the user requests.  [nebk]
C-----------------------------------------------------------------------
      ELSE
C                                      Parse given Stokes code.
         LSTOKE = STKCHR
C                                       translate general terms
         IF (STKREF.LE.-5) THEN
            IF (LSTOKE.EQ.'HALF') LSTOKE = 'XXYY'
            IF (LSTOKE.EQ.'CROS') LSTOKE = 'XYYX'
            IF (LSTOKE.EQ.'FULL') LSTOKE = 'XYXY'
         ELSE
            IF (LSTOKE.EQ.'HALF') LSTOKE = 'RRLL'
            IF (LSTOKE.EQ.'CROS') LSTOKE = 'RLLR'
            IF (STKREF.GT.0) THEN
               IF (LSTOKE.EQ.'FULL') LSTOKE = 'IQUV'
            ELSE
               IF (LSTOKE.EQ.'FULL') LSTOKE = 'RLRL'
               END IF
            END IF
         POLCOD = -1
         FORMAL = .FALSE.
         DO 100 I = 1,NOMODE
            IF (LSTOKE.EQ.POLMOD(I)) POLCOD = I
 100        CONTINUE
         IF (POLCOD.LE.0) THEN
            DO 101 I = 1,NOMODF
               IF (LSTOKE.EQ.FORMOD(I)) POLCOD = I
 101           CONTINUE
            FORMAL = .TRUE.
            END IF
C                                      Unrecognised Stokes char string.
         IF (POLCOD.LE.0) THEN
            IERR = 7
            MSGTXT = 'FNDPOL: REQUESTED STOKES STRING UNRECOGNIZED: '''
     *         // STKCHR // ''''
            GO TO 990
            END IF
C                                       (A) True Stokes input
         IF (IQUV) THEN
C                                       Test each case in POLMOD List
C                                       I or Q or U or V only
            IF ((POLCOD.GE.1) .AND. (POLCOD.LE.4)) THEN
               DO 110 I = STKREF,STKMAX,STKINC
                  IF (I.EQ.POLCOD) GO TO 999
 110              CONTINUE
               IERR = 1
C                                       IV
            ELSE IF ((POLCOD.EQ.6) .OR. (POLCOD.EQ.7)) THEN
               IF ((STKREF.NE.1) .OR. (STKDIM.NE.4)) IERR = 1
C                                       IQU
            ELSE IF (POLCOD.EQ.5) THEN
               IF ((STKREF.NE.1) .OR. (STKDIM.LT.3)) IERR = 1
C                                       QU
            ELSE IF (POLCOD.EQ.8) THEN
               IF ((STKREF.GT.2) .OR. (STKREF+STKDIM-1.LT.3) .OR.
     *            (STKINC.NE.1)) IERR = 1
C                                       IQUV, RR, LL
            ELSE IF ((POLCOD.GE.9) .AND. (POLCOD.LE.10)) THEN
               IF ((STKREF.NE.1) .OR. (STKDIM.NE.4)) IERR = 1
C                                       RL, LR
            ELSE IF ((POLCOD.EQ.11) .OR. (POLCOD.EQ.12) .OR.
     *            (POLCOD.EQ.14)) THEN
               IF ((STKREF.GT.2) .OR. (STKREF+STKDIM.LE.3)) IERR = 1
C                                       HALF, FULL
            ELSE IF ((POLCOD.EQ.13) .OR. (POLCOD.EQ.15)) THEN
               IF ((STKREF.NE.1) .OR. (STKDIM.NE.4)) IERR = 1
C                                       linears
            ELSE
               IERR = 2
               END IF
C                                       (B) R,L Combinations.
         ELSE IF (RRLL) THEN
C                                       Test each case in POLMOD List
C                                       I
            IF (POLCOD.EQ.1) THEN
               IF (STKREF.LT.-2) IERR = 1
               IF ((FORMAL) .AND. (STKREF.LT.-1)) IERR = 1
C                                       Q, U
            ELSE IF ((POLCOD.EQ.2) .OR. (POLCOD.EQ.3) .OR.
     *         (POLCOD.EQ.8)) THEN
               IF ((STKREF-STKDIM.GE.-4) .OR. (STKREF.LT.-3)) IERR = 1
C                                       V, IV
            ELSE IF ((POLCOD.EQ.4) .OR. (POLCOD.EQ.7)) THEN
               IF ((STKREF.LT.-1) .OR. (STKDIM.LT.2)) IERR = 1
C                                       IQU
            ELSE IF (POLCOD.EQ.5) THEN
               IF ((STKREF-STKDIM.GE.-4) .OR. (STKREF.LT.-2)) IERR = 1
               IF ((FORMAL) .AND. (STKREF.LT.-1)) IERR = 1
C                                       IQUV
            ELSE IF (POLCOD.EQ.6) THEN
               IF ((STKDIM.LT.4) .OR. (STKREF.NE.-1)) IERR = 1
C                                       RR or LL or RL or LR only
            ELSE IF ((POLCOD.GE.9) .AND. (POLCOD.LE.12)) THEN
               STKMCH = -POLCOD + 8
               DO 210 I = STKREF, STKMAX, STKINC
                  IF (I.EQ.STKMCH) GO TO 999
 210              CONTINUE
               IERR = 1
C                                       RR,LL (=HALF)
            ELSE IF (POLCOD.EQ.13) THEN
               IF ((STKREF.NE.-1) .OR. (STKDIM.LT.2) .OR.
     *            (STKINC.NE.-1)) IERR = 1
C                                       RL,LR (=CROS)
            ELSE IF (POLCOD.EQ.14) THEN
               IF ((STKREF-STKDIM.GT.-5) .OR. (STKINC.NE.-1)) IERR = 1
C                                       RR,LL,RL,LR (=FULL)
            ELSE IF (POLCOD.EQ.15) THEN
               IF ((STKREF.NE.-1) .OR. (STKDIM.LT.4) .OR.
     *            (STKINC.NE.-1)) IERR = 1
C                                       linears
            ELSE
               IERR = 2
               END IF
C                                       (C) X,Y Combinations.
         ELSE IF (XXYY) THEN
C                                       Test each case in POLMOD List
C                                       I
            IF (POLCOD.EQ.1) THEN
               IF (STKREF.LT.-6) IERR = 1
               IF ((FORMAL) .AND. (STKREF.LT.-5)) IERR = 1
C                                       XX or YY or XY or YX only
            ELSE IF ((POLCOD.GE.16) .AND. (POLCOD.LE.19)) THEN
               STKMCH = -POLCOD + 11
               DO 310 I = STKREF,STKMAX,STKINC
                  IF (I.EQ.STKMCH) GO TO 999
 310              CONTINUE
               IERR = 1
C                                       XXYY (=HALF)
            ELSE IF (POLCOD.EQ.20) THEN
               IF ((STKREF.NE.-5) .OR. (STKDIM.LT.2) .OR.
     *            (STKINC.NE.-1)) IERR = 1
C                                       XYYX (=CROS)
            ELSE IF (POLCOD.EQ.21) THEN
               IF ((STKREF-STKDIM.GT.-9) .OR. (STKINC.NE.-1)) IERR = 1
C                                       XX, YY, XY, YX (=FULL)
            ELSE IF (POLCOD.EQ.22) THEN
               IF ((STKREF.NE.-5) .OR. (STKDIM.LT.4) .OR.
     *            (STKINC.NE.-1)) IERR = 1
C                                       most no can do
            ELSE
               IERR = 2
               END IF
            END IF
         IF (IERR.EQ.1) THEN
            WRITE (MSGTXT,1400) STKCHR
         ELSE
            MSGTXT = 'FNDPOL: CONVERSION FROM/TO LINEARS IS' //
     *         ' UNSUPPORTED'
            END IF
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDPOL: AXEFND error = ',I3)
 1001 FORMAT ('FNDPOL: STOKES AXIS HAS AN ILLEGAL DIMENSION =',I5)
 1002 FORMAT ('FNDPOL: STOKES AXIS HAS AN ILLEGAL INCREMENT =',I5)
 1400 FORMAT ('FNDPOL: REQUESTED STOKES ',A4,
     *   ' INCOMPATIBLE WITH ACTUAL')
      END
