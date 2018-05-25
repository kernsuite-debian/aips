      SUBROUTINE ZTKBUF (IN, IT, IERR)
C-----------------------------------------------------------------------
C! flush TK buffer if needed, then store 8-bit byte in buffer
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   ZTKBUF will flush the Tektronix output buffer TKBUFF, if necessary,
C   then store the low order 8-bit byte of IN into the proper 8-bit byte
C   of TKBUFF.  This is a Z-routine to allow for any required local
C   conversions.
C   Inputs:
C      IN       I   Word from which to extract the low order byte and
C                   store in TKBUF
C      IT       I   Data type indicator: 1 => control
C                      2 => position,  3 => char
C   Output:
C      IERR     I   Error return code: 0 => no error
C                      > 1 => write error (from TEKFLS)
C   Common: (DTKS.INC)
C      TKSIZE   I      Size of TKBUFF in floating point words
C      TKBUFF   R(*)   Tektronix output buffer
C      TKPOS    I      Current byte position in TKBUFF
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   IN, IT, IERR
C
      INTEGER   IBYTE, IWORD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       TKBUFF full?
      IF (TKPOS.GE.TKSIZE*4) CALL TEKFLS (IERR)
      IF (IERR.NE.0) IERR = 1
C                                       Increment position in TKBUFF.
      TKPOS = TKPOS + 1
      IWORD = (TKPOS - 1) / 4 + 1
      IBYTE = MOD (TKPOS-1, 4) + 1
C                                       Store low order byte of IN
C                                       in IBYTE of TKBUFF(IWORD).
      CALL ZPUTCH (IN, TKBUFF(IWORD), IBYTE)
C
 999  RETURN
      END
