      SUBROUTINE YCURSE (OP, WAIT, CORR, RPOS, QUAD, BUTTON, IERR)
C-----------------------------------------------------------------------
C! read and control TV cursor
C# Y0 TV-util
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
C   YCURSE reads cursor positions and controls the blink and visibility
C   of the TV cursor.
C   Inputs:
C      OP      C*4     'READ' read cursor position
C                      'ONNN' place cursor at RPOS & leave on
C                      'OFFF' turn cursor off
C                      'BLNK' reverse sense of cursor blink
C                      'FXIT' fix RPOS for zoom scroll, no IO
C      WAIT    L       wait for event; then return RPOS & BUTTON
C                      (done on all OPs)
C      CORR    L       T => correct RPOS for zoom & scroll
C   In/Out:
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C      QUAD    I       TV quadrant to use for scrolls
C                         In: if <1 >4, no scroll
C                         Out: if in=-1, no scroll, else find
C                              quadrant (needs real TV pos)
C   Output:
C      BUTTON  I       event # (0 none, 1-7 low buttons,
C                      8-15 the "quit" button)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      REAL      RPOS(2)
      INTEGER   BUTTON, IERR, QUAD
      LOGICAL   WAIT, CORR
C
      CHARACTER OPCODE(5)*4
      REAL      T
      INTEGER   NOP, IOP, IROUND
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
      DATA NOP, OPCODE /5, 'READ','ONNN','OFFF','BLNK','FXIT'/
C-----------------------------------------------------------------------
C                                        get opcode
      IERR = 2
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       do call
 20   BUFFER(1) = IOP - 1
      BUFFER(2) = 0
      IF (WAIT) BUFFER(2) = 1
      BUFFER(3) = 0
      IF (CORR) BUFFER(3) = 1
      BUFFER(4) = RPOS(1)
      T = 1000.0 * (RPOS(1) - BUFFER(4))
      BUFFER(5) = IROUND (T)
      BUFFER(6) = RPOS(2)
      T = 1000.0 * (RPOS(2) - BUFFER(6))
      BUFFER(7) = IROUND (T)
      BUFFER(8) = QUAD
      BUFFER(9) = BUTTON
      BUFSW = 9
      BUFSR = 9
      CALL MKYBUF ('READ', 'YCURSE', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       return outputs
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         RPOS(1) = BUFFER(4) + BUFFER(5) / 1000.0
         RPOS(2) = BUFFER(6) + BUFFER(7) / 1000.0
         QUAD = BUFFER(8)
         BUTTON = BUFFER(9)
         END IF
C
 999  RETURN
      END
