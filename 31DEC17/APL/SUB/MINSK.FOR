      SUBROUTINE MINSK (LUN, FIND, LROW, NROW, ISTRT, NSKIP, BUFF,
     *   XBUFSZ, BO, NBUF, IERR)
C-----------------------------------------------------------------------
C! inits use of MSKIP to read noncontiguous, evenly spaced rows in a map
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
C   MINSK initializes the use of MSKIP to read noncontigious but evenly
C   spaced rows in a map.  Read is double buffered if possible; in which
C   case MINSK initiates the first read.  Single buffering is used if
C   the desired data cannot be double buffered.  If more data is
C   required than will fit in the buffer, multiple (NBUF) equally filled
C   buffers are obtained by NBUF calls to MSKIP.
C   Inputs:
C      LUN    I     Logical unit number.
C      FIND   I     pointer to FTAB returned by ZOPEN.
C      LROW   I     Length of a row in pixels.
C      NROW   I     Total number of rows this plane.
C      ISTRT  I     First row for read.
C      NSKIP  I     Number of rows to skip.
C      BUFF   R(*)  Output buffer.
C      XBUFSZ  I     Buffer size in AIPS bytes.
C      BO     I     Block offset
C      NBUF   I     factor times which LROW is multiplied normally = 1.
C   OUTPUT:
C     NBUF    I     number of buffer fulls to complete read of row.
C                   MSKIP must be called this number of times to
C                   complete the read.
C     IERR    I     Error code: 0 = OK
C                        1 = file not open
C                        2 = input error
C                        4 = tried to read past end of map.
C                      10+ = 10 + ZMIO or ZWAIT error.
C   FTAB assigments:
C              0 = LUN
C              1 =
C              2 = BO block offset
C              3 =
C              4 = length of row / [5] in bytes
C              5 = multiplier of [4]
C              6 = next record number.
C              7 = record increment+1 (total increment)
C              8 = # calls per record.
C              9 = record call # (when MSKIP is called)
C             10 = bytes / call
C             11 = buffer flag, -1= single, 1=>current buffer is 1
C                  2=>current buffer=2 (buffer already read)
C             12 = buffer size in pixels (1/2 for double buffering)
C             13 = NROW (the number of rows to read)
C             14 = BTYOFF the byte offset when double buffering.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, LROW, NROW, ISTRT, NSKIP, XBUFSZ, BO, NBUF,
     *   IERR
      REAL      BUFF(*)
      INTEGER   NBYTES, NSEC, IER, KBUFSZ, XNBUF, TEMP, BUFSZ
      DOUBLE PRECISION DPTEMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Check input values.
      IERR = 2
      CALL POWER2 (XBUFSZ, BUFSZ)
C                                       See if first row.GT.last row.
      IF (ISTRT.GT.NROW) GO TO 999
C                                       See if file open.
      IERR = 1
      IF (LUN.NE.FTAB(FIND)) GO TO 999
      IERR = 0
C                                       Store input values.
      FTAB(FIND+2) = BO
      FTAB(FIND+6) = ISTRT
      FTAB(FIND+7) = NSKIP
      FTAB(FIND+9) = 1
      FTAB(FIND+11) = -1
      FTAB(FIND+12) = BUFSZ / 2
      FTAB(FIND+13) = NROW
      FTAB(FIND+14) = 0
C                                       See if double, single or
C                                       multiple buffering.
      XNBUF = 2 * LROW * NBUF
C                                       Check if XNBUF an exact multiple
C                                       of NBPS.
      TEMP = MOD (XNBUF, NBPS)
      KBUFSZ = 0
      IF (ABS(TEMP).GE.1) KBUFSZ = NBPS
C                                       Double buffer, fill FTAB.
      IF (((XNBUF+KBUFSZ)*2).LE.BUFSZ) THEN
         FTAB(FIND+4) = XNBUF
         FTAB(FIND+5) = 1
         FTAB(FIND+6) = FTAB(FIND+6) + NSKIP
         FTAB(FIND+8) = 1
         FTAB(FIND+10) = XNBUF
         FTAB(FIND+11) = 1
         FTAB(FIND+12) = BUFSZ / 4
         FTAB(FIND+13) = FTAB(FIND+13) + ISTRT
C                                       Initiate first read.
C                                       Get byte offset.
         DPTEMP = (ISTRT-1) * XNBUF
         DPTEMP = DPTEMP / NBPS + BO
         NSEC = DPTEMP
         FTAB(FIND+14) = (DPTEMP-NSEC) * NBPS + 0.3D0
C                                       Do I/O
         NBYTES = FTAB(FIND+14) + FTAB(FIND+10)
         CALL ZMIO ('READ', LUN, FIND, NSEC, NBYTES, BUFF, 1, IER)
         IERR = IERR + 10 * IER
         GO TO 999
      ELSE
C                                       Single buffer or multbuffer
C                                       Multiple buffer loads, see how
C                                       many.
         NBUF = (XNBUF + KBUFSZ - 1) / BUFSZ + 1
         NBUF = MAX (1, NBUF)
C                                          Fill FTAB.
         FTAB(FIND+4) = XNBUF / (2 * NBUF)
         FTAB(FIND+5) = 2 * NBUF
         FTAB(FIND+8) = NBUF
         FTAB(FIND+10) = XNBUF / NBUF
         END IF
C
 999  RETURN
      END
