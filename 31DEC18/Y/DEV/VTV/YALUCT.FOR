      SUBROUTINE YALUCT (OP, ARMODE, BFUNC, NFUNC, CONSTS, OUTSEL,
     *   EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL, IERR)
C-----------------------------------------------------------------------
C! drives the TV arithmetic logic unit - not to be used much
C# Y2 TV-basic
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
C   Virtual TV version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BFUNC, NFUNC, CONSTS(8), OUTSEL(8), IERR
      LOGICAL   ARMODE, EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        Bad op if no ALU units
      IERR = 2
      IF (TVALUS.LE.0) GO TO 999
C                                       set buffer
      BUFFER(1) = 0
      IF (ARMODE) BUFFER(1) = 1
      BUFFER(2) = BFUNC
      BUFFER(3) = NFUNC
      CALL COPY (8, CONSTS, BUFFER(4))
      CALL COPY (8, OUTSEL, BUFFER(12))
      BUFFER(20) = 0
      IF (EXTOFM) BUFFER(20) = 1
      BUFFER(21) = 0
      IF (ESHIFT) BUFFER(21) = 1
      BUFFER(22) = 0
      IF (SHIFT) BUFFER(22) = 1
      BUFFER(23) = 0
      IF (CARYIN) BUFFER(23) = 1
      BUFFER(24)  = 0
      BUFFER(25) = 0
      IF (OP.EQ.'WRIT') THEN
         BUFSW = 25
         BUFSR = 0
      ELSE
         BUFSR = 25
         BUFSW = 0
         END IF
C                                        header & IO
      CALL MKYBUF (OP, 'YALUCT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        read: return control parms
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         ARMODE = BUFFER(1).EQ.1
         BFUNC = BUFFER(2)
         NFUNC = BUFFER(3)
         CALL COPY (8, BUFFER(4), CONSTS)
         CALL COPY (8, BUFFER(12), OUTSEL)
         EXTOFM = BUFFER(20).EQ.1
         ESHIFT = BUFFER(21).EQ.1
         SHIFT = BUFFER(22).EQ.1
         CARYIN = BUFFER(23).EQ.1
         CARRY = BUFFER(24).EQ.1
         EQUAL = BUFFER(25).EQ.1
         END IF
C
 999  RETURN
      END
