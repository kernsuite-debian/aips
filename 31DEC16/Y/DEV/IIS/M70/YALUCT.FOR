      SUBROUTINE YALUCT (OP, ARMODE, BFUNC, NFUNC, CONSTS, OUTSEL,
     *   EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL, IERR)
C-----------------------------------------------------------------------
C! drives the TV arithmetic logic unit - not to be used much
C# TV-basic
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
C   YALUCT reads / writes the TV arithmetic logic unit control
C   registers.  The actual feedback-ALU computation is performed only
C   upon a call to YFDBCK.
C   Inputs:
C      OP      C*4     'READ' from TV or 'WRIT' to TV
C   In/Out:
C      ARMODE  L       T => arithmetic mode F => logic mode
C      BFUNC   I       function number (1-16) in blotch
C      NFUNC   I       function number (1-16) outside blotch
C      CONSTS  I(8)    constant array (may select as ALU output)
C      OUTSEL  I(8)    lookup table selects output based on carry
C                      (lsb), equal, ROI (msb) input. values -
C                      0 - 7 : constants 1 - 8
C                      8     : accumulator channel pair
C                      9     : selected OFM
C                      10    : ALU
C                      11    : external
C      EXTOFM  L       T => extend sign of OFM on input to ALU
C      ESHIFT  L       T => extend sign of ALU output if SHIFT
C      SHIFT   L       T => right shift ALU output
C      CARYIN  L       T => add one to arithmetic results
C   Output:
C      CARRY   L       T => carry condition occurred in frame
C      EQUAL   L       T => equal condition occurred in frame
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   IIS Model 70 version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BFUNC, NFUNC, CONSTS(8), OUTSEL(8), IERR
      LOGICAL   ARMODE, EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL
C
      INTEGER   HEADER(8), BUFFER(16), BUFSZ1, BUFSZ2, UNIT, BITS(16),
     *   NB, I, NOT
      LOGICAL   COMND1, COMND2, VRTRTC
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMND1, COMND2, BUFSZ1, BUFSZ2 /11,.TRUE.,.FALSE.,
     *   1,16/
      DATA VRTRTC /.FALSE./
C-----------------------------------------------------------------------
C                                        Bad op if no ALU units
      IERR = 2
      IF (TVALUS.LE.0) GO TO 999
C                                        Control register first
C                                        write: check errors
      IF (OP.EQ.'WRIT') THEN
         IF ((BFUNC.LT.1) .OR. (BFUNC.GT.16)) GO TO 999
         IF ((NFUNC.LT.1) .OR. (NFUNC.GT.16)) GO TO 999
         DO 10 I = 1,8
            IF ((OUTSEL(I).LT.0) .OR. (OUTSEL(I).GT.11)) GO TO 999
 10         CONTINUE
C                                        build word
         I = NFUNC - 1
         CALL ZGTBIT (4, I, BITS(1))
         I = BFUNC - 1
         CALL ZGTBIT (4, I, BITS(5))
         BITS(11) = 1
         BITS(15) = 0
         BITS(16) = 0
         IF (.NOT.ARMODE) BITS( 9) = 1
         IF (CARYIN) BITS(10) = 1
         IF (EXTOFM) BITS(12) = 1
         IF (SHIFT ) BITS(13) = 1
         IF (ESHIFT.AND.SHIFT) BITS(14) = 1
         CALL ZPTBIT (16, BUFFER, BITS)
         END IF
C                                        header & IO
 15   CALL YMKHDR (OP, UNIT, COMND1, BUFSZ1, 0, 0, 0, 0, VRTRTC,
     *   HEADER)
      NB = BUFSZ1 * 2
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        read: return control parms
      IF (OP.NE.'WRIT') THEN
         CALL ZGTBIT (16, BUFFER(1), BITS)
         CALL ZPTBIT (4, NFUNC, BITS(1))
         CALL ZPTBIT (4, BFUNC, BITS(5))
         NFUNC = NFUNC + 1
         BFUNC = BFUNC + 1
         ARMODE = BITS(9).NE.1
         CARYIN = BITS(10).EQ.1
         EXTOFM = BITS(12).EQ.1
         SHIFT  = BITS(13).EQ.1
         ESHIFT = BITS(14).EQ.1
         CARRY  = BITS(15).EQ.1
         EQUAL  = BITS(16).EQ.1
C                                        Data registers
C                                        YALUCR converts to needed
C                                        1's complement & back
      ELSE
         DO 25 I = 1,8
            BUFFER(I  ) = NOT (CONSTS(I))
            BUFFER(I+8) = OUTSEL(I)
 25         CONTINUE
         END IF
C                                        header & IO
 30   CALL YMKHDR (OP, UNIT, COMND2, BUFSZ2, 0, 0, 0, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSZ2
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                        return registers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         DO 35 I = 1,8
            CONSTS(I) = NOT (BUFFER(I))
            OUTSEL(I) = BUFFER(I+8)
 35         CONTINUE
         END IF
C
 999  RETURN
      END
