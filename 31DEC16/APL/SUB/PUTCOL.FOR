      SUBROUTINE PUTCOL (IRNO, ICOL, DATP, LRNO, BUFFER, RESULT, ROWB,
     *   IERR)
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
C   PUTCOL enters the value and value type into an open table file
C   at the specified logical column and row.
C   Inputs:
C      IRNO    I         Table row number (<= 0 => write ROWB to disk)
C      ICOL    I         Table column number
C      DATP    I(256)    Pointer array returned by TABINI
C      RESULT  ?(*)      Value of column: use D, R, H, I equivalenced
C                        arrays, note: may be an array; if length = 0,
C                        then no value and RESULT is unchanged.
C   In/out:
C      LRNO    I         Row number now in ROWB
C      BUFFER  I(*)      Control area set up by TABINI, used in TABIO
C      ROWB    I(*)      Buffer holding the row LRNO
C   Output:
C      IERR    I         Error code: 0 => OK.
C                          -1 => OK, but row is flagged
C                           1 file not open,  2 input error
C                           3 I/O error,      4 read past EOF
C                           5 bad data type
C-----------------------------------------------------------------------
      INTEGER   IRNO, ICOL, DATP(256), LRNO, BUFFER(512), RESULT(*),
     *   ROWB(*), IERR
C
      INTEGER   IPTR, IL, J, LENGTH, RTYPE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       put row back with new value
      IF ((IRNO.NE.LRNO) .AND. (LRNO.GT.0)) THEN
         CALL TABIO ('WRIT', 0, LRNO, ROWB, BUFFER, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1980) IERR, IRNO, ICOL
            CALL MSGWRT (8)
            IERR = 10*IERR
            GO TO 999
            END IF
         END IF
C                                       check
      IERR = 0
      IF (IRNO.LE.0) THEN
         LRNO = 0
         GO TO 999
         END IF
      IF ((ICOL.LE.0) .OR. (ICOL.GT.BUFFER(10))) THEN
         WRITE (MSGTXT, 1960) ICOL, BUFFER(10)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       get row, if needed
      IF (IRNO.NE.LRNO) THEN
C                                       read if not off end
         IF (IRNO.LE.BUFFER(5)) THEN
            CALL TABIO ('READ', 0, IRNO, ROWB, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT, 1970) IERR, IRNO, ICOL
               CALL MSGWRT (8)
               DO 50 IL = 1,10
                  WRITE (MSGTXT, 1990) IL, (BUFFER((IL-1)+J), J=1,5)
                  CALL MSGWRT (8)
 50               CONTINUE
               GO TO 999
               END IF
C                                       else zero
         ELSE
            IL = BUFFER(8) - 1
            CALL FILL (IL, 0, ROWB)
            END IF
         LRNO = IRNO
         END IF
C                                       get position of variable
      IERR = 5
      LENGTH = DATP(128+ICOL) / 10
      RTYPE = MOD (DATP(128+ICOL), 10)
C                                       For zero length return error
      IF (LENGTH.LE.0) GO TO 999
C                                       bad type
      IF ((RTYPE.LT.1) .OR. (RTYPE.GT.7)) GO TO 999
C                                       get start location of variable
      IPTR = DATP(ICOL)
C                                       calc variable length in I*4's
C                                       double precision
      IF (RTYPE.EQ.1) THEN
         IL = NWDPDP
         IPTR = ((IPTR - 1) * IL) + 1
C                                       if variable is multi dimensional
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
      CALL COPY (IL, RESULT, ROWB(IPTR))
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1960 FORMAT ('PUTCOL: BAD COLUMN ',I7,', BUFFER(10)= ',I7)
 1970 FORMAT ('PUTCOL: BAD READ, ERR= ',I3,', ROW,COL =',2I7)
 1980 FORMAT ('PUTCOL: BAD WRITE, ERR=',I3,', ROW,COL =',2I7)
 1990 FORMAT ('PUTCOL: BUFFER(',I3,')=',5I5)
      END



