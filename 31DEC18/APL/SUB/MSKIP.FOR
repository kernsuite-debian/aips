      SUBROUTINE MSKIP (LUN, FIND, BUFF, BIND, IFIN, IERR)
C-----------------------------------------------------------------------
C! reads noncontiguous, but evenly spaced rows in a map (see also MINSK)
C# AP-fft Map-util
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
C   MSKIP reads rows in a map file which are evenly spaced.  The reads
C   are double, single buffered or partial buffers if the row size
C   1) is .Le. BUFSZ/2, 2) between BUFSZ/2 and BUFSZ or 3).GT.BUFSZ.
C   For case 3) multiple calls (NBUF from MINS3) are required to read
C   each row.  Each call returns LROW*BP/NBUF bytes and I/O is
C   single buffered.  IFIN = 0 indicates a row is completed.  See
C   MINS3 for more details.
C   Input:
C      LUN    I      Logical unit number.
C      FIND   I      pointer for FTAB
C      BUFF   R(*)   Buffer
C   Output:
C      BIND   I      Pointer for BUFF
C      IFIN   I      0 if row complete, 1 otherwise.
C      IERR   I      error code: 0 = OK
C                                1 = file not open
C                                2 = attempt to read past end of map.
C                               10+= I/O error = 10 +  ZWAIT error.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, BIND, IFIN, IERR
      REAL      BUFF(*)
C
      INTEGER   IER, NSEC, BYTOFF, IBUF, JBUF, KBUF, IP, NBYTES
      DOUBLE PRECISION DPTEMP
      LOGICAL   DOUBUF
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Check for file open.
      IERR = 1
      IF (FTAB(FIND).NE.LUN) GO TO 999
C                                       Check if next read past end
C                                       of map
      IERR = 2
      IF (FTAB(FIND+6).GT.FTAB(FIND+13)) GO TO 999
      IERR = 0
      DOUBUF = FTAB(FIND+11).GT.0
C                                       Set buffer pointers.
      JBUF = ABS (FTAB(FIND+11))
      KBUF = 1
      IF (DOUBUF) KBUF = 3 - JBUF
      IBUF = (KBUF - 1) * FTAB(FIND+12)
      IP = (IBUF / 2) * 2 + 1
C                                       If double buffering,
C                                       skip last read.
      IF ((DOUBUF) .AND. (FTAB(FIND+6).GE.FTAB(FIND+13))) GO TO 40
C                                       Compute position in file.
         DPTEMP = (FTAB(FIND+6) - 1) * FTAB(FIND+4) * FTAB(FIND+5)
         DPTEMP = ((DPTEMP + (FTAB(FIND+9) - 1) * FTAB(FIND+10)) /
     *      NBPS) + FTAB(FIND+2)
         NSEC = DPTEMP
C                                       Compute byte offset.
         BYTOFF = (DPTEMP-NSEC) * NBPS + 0.3D0
C                                       Compute # bytes.
         NBYTES = BYTOFF + FTAB(FIND+10)
C                                       Do I/O
         CALL ZMIO ('READ', LUN, FIND, NSEC, NBYTES, BUFF(IP),
     *      KBUF, IER)
         IERR = IERR + 10 * IER
         IF (IER.NE.0) GO TO 999
C                                       Update counters.
 40   FTAB(FIND+9) = FTAB(FIND+9) + 1
C                                       See if finished with record.
      IFIN = 1
      IF (FTAB(FIND+9) .LE. FTAB(FIND+8)) GO TO 50
         FTAB(FIND+9) = 1
         FTAB(FIND+6) = FTAB(FIND+6) + FTAB(FIND+7)
         IFIN = 0
C                                       Set output buffer pointer.
 50   IF (DOUBUF) GO TO 60
         BIND = BYTOFF / 2 + 1
         GO TO 70
C                                       Double buffer.
 60   FTAB(FIND+11) = KBUF
      BIND = FTAB(FIND+14) / 2 + 1 + (JBUF-1) * FTAB(FIND+12)
      FTAB(FIND+14) = BYTOFF
C                                       Wait for I/O
 70   CALL ZWAIT (LUN, FIND, JBUF, IER)
      IERR = IERR + 10 * IER
C
 999  RETURN
      END
