      SUBROUTINE YSCROL (CHANNL, SCROLX, SCROLY, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! write the scroll registers (shift location of 1 or more TV channels)
C# Y1 TV-basic
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
C   YSCROL writes the scroll registers on the TV.
C   Inputs:
C      CHANNL   I   bit map channel select: bits 1-NGRAY gray channels,
C                   bit NGRAY+1 => all graphics
C      VRTRTC   L   T => do it on vertical retrace only
C   In/Out:
C      SCROLX   I   amount of X scroll (>0 to right)
C      SCROLY   I   amount of Y scroll (>0 upwards)
C   Output:
C      IERR     I   error from Z...XF : 0 => ok, 2 => input error
C   YSCROL updates the scroll variables in /TVDEV/ common
C   Modified Nov-83 for DeAnza by Jaffe, corrected EWG 12/85.
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:YDEA.INC'
      INTEGER   IX, BUFFER(2), BITS(16), I, J, ZREG(2), SHIFT, IMAG,
     *   JMAG, KOFF, SCMASK, ITEMP, IC, IXSCRL, IYSCRL, ZAND
      LOGICAL   UPDATE, WAIT
      INCLUDE 'INCS:DTVC.INC'
      DATA WAIT /.TRUE./
C     DATA SHIFT, SCMASK /Z0020, Z03FF/
      DATA SHIFT, SCMASK /32,    1023/
C-----------------------------------------------------------------------
      IXSCRL = MOD (SCROLX, MAXXTV(1))
      IYSCRL = MOD (SCROLY, MAXXTV(2))
      BUFFER(1) = - IXSCRL
      BUFFER(2) = - IYSCRL
      BUFFER(1) = (BUFFER(1) / SCXINC) * SCXINC
      BUFFER(2) = (BUFFER(2) / SCYINC) * SCYINC
C                                       scroll equiv of current zoom
      IMAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) IMAG = 2**TVZOOM(1)
C                                       Shift zoom factor to high byte
      JMAG = SHIFT * (IMAG - 1)
      DO 10 I = 1,2
         ZREG(I) = 0
         CALL ZPUTCH (JMAG, ZREG(I), 2)
 10      CONTINUE
C
      IX = REAL(TVZOOM(2)-1) * REAL(IMAG-1) /REAL(IMAG)
C                                       Add scroll &
C                                       clear high order bits
      ITEMP = BUFFER(1) + IX
      BUFFER(1) = ZAND (ITEMP, SCMASK)
      IX = (REAL (TVZOOM(3)-1) * REAL(IMAG-1) + REAL(MAXXTV(2) - 1))
     *   / REAL(IMAG)
C                                       Add scroll &
C                                       clear high order bits
      ITEMP = BUFFER(2) + IX
      BUFFER(2) = ZAND (ITEMP, SCMASK)
C                                       Loop over channels
      ITEMP = 2 ** NGRAY - 1
      IC = ZAND (CHANNL, ITEMP)
      CALL ZGTBIT (16, IC, BITS)
      IF (IC.NE.CHANNL) BITS(NGRAY+1) = 1
      IC = NGRAY + 1
      DO 20 I = 1,IC
         IF (BITS(I).NE.1) GO TO 20
            TVSCRX(I) = IXSCRL
            TVSCRY(I) = IYSCRL
            UPDATE = .FALSE.
            KOFF = 4 *(I-1)
C                                        Have I changed X or Y ?
            DO 15 J = 1,2
               ITEMP = BUFFER(J) + ZREG(J)
               CALL YTCOMP (YBUFF(XSCZ+KOFF+J-1), ITEMP, UPDATE)
 15            CONTINUE
C                                        If so write it into memory LUT
            ITEMP = I - 1
            IF (UPDATE) CALL ZDEAXF ('LMC ', YBUFF(XSCZ+KOFF), 8,
     *         ITEMP, 0, WAIT, IERR)
            IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C
 999  RETURN
      END
