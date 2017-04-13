      SUBROUTINE GETCOL (IRNO, ICOL, DATP, LRNO, BUFFER, RTYPE, RESULT,
     *   ROWB, IERR)
C-----------------------------------------------------------------------
C! returns value and type found at specified column and row in a table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   GETCOL returns the value and value type found in an open table file
C   at the specified logical column and row.
C   Inputs:
C      IRNO    I         Table row number: n.b. I
C      ICOL    I         Table column number
C      DATP    I(256)    Pointer array returned by TABINI
C   In/out:
C      LRNO    I         Last read row number: set to 0 if ROWB is
C                        meaningless on input.  In/output value < 0 =>
C                        row abs(LRNO) flagged.
C      BUFFER  I(512)    Control area set up by TABINI, used in TABIO
C      ROWB    I(*)      Buffer to hold one full row: used on input if
C                        LRNO = IRNO
C   Output:
C      RTYPE   I         10 * length + Type of column:
C                        1 -> D,  2 -> R,  4 -> I,  5 -> L*?,  6 -> I.
C                        Characters unpacked hollerith, bits packed.
C      RESULT  ?(*)      Value of column: use D, R, H, I equivalenced
C                        arrays, note: may be an array; if length = 0,
C                        then no value and RESULT is unchanged.
C      IERR    I         Error code: 0 => OK.
C                          -1 => OK, but row is flagged
C                           1 file not open,  2 input error
C                           3 I/O error,      4 read past EOF
C                           5 bad data type
C-----------------------------------------------------------------------
      INTEGER   IRNO, ICOL, DATP(256), LRNO, BUFFER(512), RTYPE,
     *   RESULT(*), ROWB(*), IERR
C
      INTEGER   IPTR, IL, LENGTH
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       check
      IERR = 2
      IF (IRNO.LE.0) GO TO 999
      IF ((ICOL.LE.0) .OR. (ICOL.GT.BUFFER(10))) GO TO 999
      IERR = 0
C                                       get row
      IF (IRNO.NE.ABS(LRNO)) THEN
         CALL TABIO ('READ', 0, IRNO, ROWB, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 999
         LRNO = IRNO
         IF (IERR.LT.0) LRNO = -IRNO
      ELSE
         IF (LRNO.LT.0) IERR = -1
         END IF
C                                       get type, I   address, length
      LENGTH = DATP(128+ICOL) / 10
      RTYPE = DATP(128+ICOL) - LENGTH * 10
C                                       For zero length return
      IF (LENGTH.LE.0) GO TO 999
      IPTR = DATP(ICOL)
C                                       bad  type
      IF ((RTYPE.LT.1) .OR. (RTYPE.GT.7)) THEN
         IERR = 5
         GO TO 999
         END IF
C                                       double precision
      IF (RTYPE.EQ.1) THEN
         IL = NWDPDP
         IPTR = (IPTR - 1) * IL + 1
         IL = IL * LENGTH
C                                       Characters
      ELSE IF (RTYPE.EQ.3) THEN
         IL = (LENGTH-1) / 4 + 1
C                                       Bit
      ELSE IF (RTYPE.EQ.7) THEN
         IL = (LENGTH - 1) / NBITWD + 1
C                                       integer, floating
      ELSE
         IL = LENGTH
         END IF
C                                       copy answer
      CALL COPY (IL, ROWB(IPTR), RESULT)
      RTYPE = RTYPE + 10 * LENGTH
C
 999  RETURN
      END
