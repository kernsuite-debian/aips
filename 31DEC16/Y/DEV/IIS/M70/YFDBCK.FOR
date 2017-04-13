      SUBROUTINE YFDBCK (COLOR, CHANNL, BITPL, PIXOFF, BYPIFM, EXTERN,
     *   ZERO, ACCUM, ADDWRT, IERR)
C-----------------------------------------------------------------------
C! causes a feedback operation in the TV
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
C   YFDBCK sends a feedback command to the TV
C   Inputs:
C      COLOR   I    bit map of color to be fedback (RGB = 4,2,1)
C      CHANNL  I    bit map of channels to receive feedback
C      BITPL   I    bit map of bit planes to receive feedback
C      PIXOFF  I    offset fedback image to left by 0 - 1 pixels
C      BYPIFM  L    F => image goes thru IFM lookup before store
C      EXTERN  L    T => image from external input (iedigitizer)
C      ZERO    L    T => feed back all zeros
C      ACCUM   L    T => use 16-bit accumulator mode then CHANNL must
C                        give even-odd pair lsbyte goes to even (lower)
C                        # channel
C      ADDWRT  L    T => additive write  F => replace old data
C   Outputs:
C      IERR    I    error code of Z...XF: 0 -> ok
C                                         2 -> input error
C   IIS Model 70 version
C-----------------------------------------------------------------------
      INTEGER   COLOR, CHANNL, BITPL, PIXOFF, IERR
      LOGICAL   BYPIFM, EXTERN, ZERO, ACCUM, ADDWRT
C
      INTEGER   NB, UNIT, HEADER(8), BUFFER, BUFSIZ, BITS(16)
      LOGICAL   COMAND, VRTRTC
      DATA UNIT, COMAND, VRTRTC /5,.FALSE.,.FALSE./
      DATA BUFSIZ /1/
C-----------------------------------------------------------------------
C                                        check error if not ZERO
      IF (ZERO) GO TO 10
         IERR = 2
         IF ((PIXOFF.LT.0) .OR. (PIXOFF.GT.1)) GO TO 999
         IF ((.NOT.EXTERN) .AND. (COLOR.NE.1) .AND. (COLOR.NE.2) .AND.
     *      (COLOR.NE.4)) GO TO 999
C                                        Standard header
 10   CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, CHANNL, BITPL,
     *   VRTRTC, HEADER)
C                                        header controls
      CALL ZGTBIT (16, HEADER(1), BITS)
      IF (BYPIFM) BITS(14) = 1
      IF (ADDWRT) BITS(12) = 1
      IF (ACCUM)  BITS(11) = 1
      BITS(13) = 1
      BITS(10) = 1
      CALL ZPTBIT (16, HEADER(1), BITS)
C                                        data word
      CALL FILL (16, 0, BITS)
      CALL ZGTBIT (3, COLOR, BITS(1))
      CALL ZGTBIT (1, PIXOFF, BITS(9))
      IF (EXTERN) BITS(15) = 1
      IF (ZERO  ) BITS(16) = 1
      CALL ZPTBIT (16, BUFFER, BITS)
C                                        IO
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C
 999  RETURN
      END
