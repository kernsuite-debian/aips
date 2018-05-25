      SUBROUTINE DCODEF (IPOS, IWIDTH, IFRAC, LINE, VALUE, IERR)
C-----------------------------------------------------------------------
C! Decodes data from a character string using a format.
C# Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Decode the number.
C   Inputs:
C      IPOS    I      Starting position of number in LINE.
C      IWIDTH  I      Width of field.
C      IFRAC   I      Number of fraction positions.  Works like FORTRAN
C                     format Fw.f  where "w" is IWIDTH and "f" is IFRAC
C      LINE   C*(*)   Character string.
C   Output:
C      VALUE   D      Output value.
C      IERR    I      Error code: 0 = okay
C-----------------------------------------------------------------------
      CHARACTER LINE*(*), DIGIT*10, ONECH*1
      INTEGER   IPOS, IWIDTH, IFRAC, IERR
      DOUBLE PRECISION VALUE, SIGN
      INTEGER   IT, NFRAC, IPOWER, IESIGN, ISTART, IEND, I, ISTATE
      LOGICAL   FRCFLG, INT
      INCLUDE 'INCS:DMSG.INC'
      DATA DIGIT /'0123456789'/
C-----------------------------------------------------------------------
C                                       Set some defaults.
      VALUE = 0.0D0
      NFRAC = 0
      FRCFLG = .FALSE.
      IPOWER = 0
      SIGN = 1.0D0
      ISTATE = 1
      IESIGN = 1
      ISTART = IPOS
      IEND = ISTART + IWIDTH - 1
      INT = .FALSE.
C                                       Integer format
      IF (IFRAC.GE.0) GO TO 10
         FRCFLG = .TRUE.
         INT = .TRUE.
 10   DO 600 I = ISTART,IEND
         ONECH = LINE(I:I)
         GO TO (100, 200, 300, 400, 500, 600), ISTATE
C                                       Look for sign.
 100        IF (ONECH.EQ.' ') GO TO 600
C                                       Plus
            IF (ONECH.NE.'+') GO TO 110
               ISTATE = 2
               GO TO 600
C                                       Minus
 110        IF (ONECH.NE.'-') GO TO 200
               SIGN = -1.0D0
               ISTATE = 2
               GO TO 600
C
 200        IF (INT) GO TO 300
C                                       DOT (starts fractional part)
            IF (ONECH.NE.'.') GO TO 300
               ISTATE = 3
               FRCFLG = .TRUE.
               GO TO 600
C
 300        IT = 0
            IF (ONECH.EQ.' ') GO TO 320
               IT = INDEX (DIGIT, ONECH) - 1
 320        IF ((IT.LT.0).OR.(IT.GT.9)) GO TO 350
               VALUE = VALUE * 10 + IT
               ISTATE = MAX (ISTATE, 2)
               IF (ISTATE.EQ.3) NFRAC = NFRAC + 1
               GO TO 600
C
 350        IF (INT) GO TO 980
C                                       E (starts exponent).
            IF ((ONECH.NE.'E').AND.(ONECH.NE.'e').AND.
     *         (ONECH.NE.'D').AND.(ONECH.NE.'d')) GO TO 980
               ISTATE = 4
               GO TO 600
C                                       Look for exponent sign.
 400        IF (ONECH.EQ.' ') GO TO 600
            ISTATE = 5
            IF (ONECH.EQ.'+') GO TO 600
C                                       Minus
            IF (ONECH.NE.'-') GO TO 500
               IESIGN = -1
               GO TO 600
C                                       Look for power.
 500        IT = 0
            IF (ONECH.EQ.' ') GO TO 520
               IT = INDEX (DIGIT, ONECH) - 1
 520        CONTINUE
            IF ((IT.LT.0).OR.(IT.GT.9)) GO TO 980
            IPOWER = 10 * IPOWER + IT
 600        CONTINUE
C                                       Calculate number.
      IF (.NOT.FRCFLG) NFRAC = IFRAC
      VALUE = SIGN * VALUE * 10.0D0 ** (IESIGN * IPOWER - NFRAC)
      GO TO 999
C                                       Error In format.
 980  IERR = 1
      WRITE (MSGTXT,1980) LINE(ISTART:IEND)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('COULD NOT DECODE ',A)
      END
