      SUBROUTINE SETSTK (STOKES, DOCAL, J)
C-----------------------------------------------------------------------
C! Sets STOKES parameters correctly for plotting routines
C# UV Calibration EXT-appl Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2014
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
C   A routine to examine the STOKES adverb passed to a task and modify
C   it if necessary to ensure that the one STOKES desired by the user is
C   the one returned from UVGET.  This is necessary to avoid the
C   occasional problem arising from weights being set for some
C   correlators and not others.
C   Input:
C      DOCAL    L       Logical specifying whether the data
C                       is to be calibrated.
C   Input/Output:
C      STOKES   C*4     Input stokes adverb, on output is the
C                       one that should be used by UVGET.
C   Input from common /UVHDR/
C      ICOR0    I       Stokes type in catalogue header
C   Output:
C      J        I       Pointer to the array CHSTOK defining the
C                       Stokes value used:
C                       1 = I,   2 = Q,    3 = U,   4 = V,
C                       5 = IQU, 6 = IQUV, 7 = IV,
C                       8 = RR,  9 = LL,  10 = RL, 11 = LR,
C                      12 = HALF (i.e. RR,LL)
C                      13 = FULL (i.e. RR,LL,RL,LR)
C
C  Rules used:
C    (1) If STOKES = IQU, IQUV or IV, set STOKES = I
C    (2) If STOKES undefined, set STOKES = RR or LL or XX or YY unless
C        calibrating and > 1 correlator, or STOKES already converted,
C        or no Source random parameter then set STOKES = I
C    (3) If STOKES = HALF or FULL, set STOKES = RR or LL or XX or YY
C        unless STOKES already converted then set STOKES = I
C-----------------------------------------------------------------------
      CHARACTER STOKES*4
      INTEGER   J
      LOGICAL   DOCAL
C
      CHARACTER CHSTOK(18)*4
      INTEGER   NSTOK, I
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NSTOK /14/
      DATA CHSTOK /'I   ','Q   ','U   ','V   ','IQU ','IQUV','IV  ',
     *   'RR  ','LL  ','RL  ','LR  ','HALF','FULL','    ',
     *   'XX', 'YY', 'XY', 'YX'/
C-----------------------------------------------------------------------
      J = 0
      DO 100 I = 1,NSTOK
         IF (STOKES.EQ.CHSTOK(I)) J = I
 100     CONTINUE
      IF ((J.GE.5) .AND. (J.LE.7)) J = 1
C                                       unspecified
      IF ((J.LE.0) .OR. (J.EQ.14)) THEN
         IF (ICOR0.GT.-5) THEN
            J = 8
            IF (ICOR0.LT.-1) J = 9
            IF ((DOCAL.AND.(NCOR.GT.1)) .OR. (ICOR0.GT.0) .OR.
     *         (ILOCSU.LT.0)) J = 1
         ELSE
            J = 15
            IF (ICOR0.LT.-5) J = 16
            IF ((DOCAL.AND.(NCOR.GT.1)) .OR. (ICOR0.GT.0) .OR.
     *         (ILOCSU.LT.0)) J = 1
            END IF
         END IF
C                                       half, full
      IF ((J.EQ.12) .OR. (J.EQ.13)) THEN
         IF (ICOR0.GT.0) THEN
            J = 1
         ELSE IF (ICOR0.GT.-5) THEN
            J = 8
            IF (ICOR0.LT.-1) J = 9
         ELSE
            J = 15
            IF (ICOR0.LT.-5) J = 16
            END IF
         END IF
C                                       warning message
      IF ((.NOT.DOCAL) .AND. (ICOR0.LE.0)) THEN
         IF ((J.LE.7) .AND. (ILOCSU.GE.0)) THEN
            WRITE (MSGTXT,1000)
            CALL MSGWRT (2)
            END IF
         END IF
C                                       Stokes to use
      STOKES = CHSTOK(J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING: CONVERSION TO STOKES BEFORE CALIBRATION',
     *   ' IS MEANINGLESS')
      END

