      SUBROUTINE MDISK (OP, LUN, FIND, BUFF, BIND, IERR)
C-----------------------------------------------------------------------
C! reads or writes a row from an image
C# Map-util IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   MDISK reads or writes a row of an image.  MDISK is called only after
C   a call to MINIT and you should read the precursor remarks of MINIT.
C   MDISK actually sets an array index (BIND) to the start of the next
C   line wanted.  Actual IO is done only when needed and a row is
C   written not on "its" call to MDISK but on some subsequent call (or
C   the FINI call.  NOTE: the line sequence is set by the WIN parameter
C   in the MINIT call.
C   Inputs:
C      OP    C*4   Op code 'WRIT', 'READ', 'FINI' (flush write buffers)
C      LUN   I     logical unit number
C      FIND  I     Pointer to FTAB returned by ZOPEN
C   Input and output:
C      BUFF  R(*)  Buffer holding data
C   Output:
C      BIND  I     Pointer to position in buffer of first pixel in
C                  window in the present line
C      IERR  I     Error return: 0 => ok
C                        1 => file not open
C                        2 => input error
C                        3 => I/O error
C                        4 => end of file
C                        5 => beginning of medium
C                        6 => end of medium
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, BIND, IERR
      REAL      BUFF(*)
C
      INTEGER   IOP, NBYTE, JBUF, FNX, FXC, FYC, FIBUF, FIXC, FBBLK,
     *   DBUF, IBUF, FUNF, FIBLK, FNBYTE, IWRITE, IREAD, IFINI
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IWRITE, IREAD, IFINI /1, 2, 4/
C-----------------------------------------------------------------------
C                                       decode OPcode
      IOP = 0
      IF (OP.EQ.'WRIT') IOP = IWRITE
      IF (OP.EQ.'READ') IOP = IREAD
      IF (OP.EQ.'FINI') IOP = IFINI
      IERR = 2
      IF ((IOP.NE.FTAB(FIND+11)) .AND. (IOP.NE.IFINI)) GO TO 999
      IF ((IREAD.EQ.FTAB(FIND+11)) .AND. (IOP.EQ.IFINI)) GO TO 999
C                                       check if file open
      IERR = 1
      IF (FTAB(FIND).NE.LUN) GO TO 999
      IERR = 0
C                                       set pointers to FTAB
      FNX = FIND + 1
      FXC = FIND + 2
      FYC = FIND + 3
      FIBUF  = FIND + 6
      FBBLK  = FIND + 7
      FIBLK  = FIND + 9
      FNBYTE = FIND + 10
      FIXC = FIND + 14
      IBUF   = FTAB(FIBUF)
C                                       which buffer ???
      JBUF = IBUF
      IF (JBUF.LT.0) JBUF = -JBUF
 20   CONTINUE
         DBUF = (JBUF - 1) * ((1+FTAB(FNBYTE))/2)
C                                       FINI: decr XCount, reset YCount
         IF (IOP.EQ.IFINI) THEN
            FTAB(FIXC) = FTAB(FIXC) - 1
            IF (FTAB(FIXC).LE.0) FTAB(FXC) = FTAB(FXC) - 1
            FTAB(FYC) = FTAB(FXC)
C                                       other: stay in buffer?
         ELSE
            IF (FTAB(FXC).LE.FTAB(FNX)) GO TO 200
            END IF
C                                       prepare to read next buffer
C                                       if all lines in, just flip bufs
         IF (FTAB(FYC).GT.0) THEN
            NBYTE = FTAB(FNBYTE)
            FUNF = FTAB(FNX) - FTAB(FYC)
C                                       finish the IO now
            IF (FUNF.GT.0) THEN
               IF (FTAB(FIBLK).GE.0) THEN
                  NBYTE = NBYTE -FUNF* FTAB(FIBLK) * NBPS
C                                       modify to finish backwards cycle
               ELSE
                  NBYTE = NBYTE + FUNF * FTAB(FIBLK) * NBPS
                  DBUF = DBUF - (FUNF*NBPS*FTAB(FIBLK))/2
                  FTAB(FBBLK) = FTAB(FBBLK) - FUNF * FTAB(FIBLK)
                  END IF
               END IF
C                                       do I/O required
            IF (IOP.EQ.IREAD) CALL ZMIO ('READ', LUN, FIND, FTAB(FBBLK),
     *         NBYTE, BUFF(1+DBUF), JBUF, IERR)
            IF ((IOP.EQ.IWRITE) .OR. (IOP.EQ.IFINI)) CALL ZMIO ('WRIT',
     *         LUN, FIND, FTAB(FBBLK), NBYTE, BUFF(1+DBUF), JBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            FTAB(FIND+12) = FTAB(FIND+12) + JBUF
C                                       if FINI, don't flip buffer
            IF (IOP.EQ.IFINI) GO TO 140
C                                       update begin block & Y counters
            FTAB(FBBLK) = FTAB(FBBLK) + FTAB(FNX) * FTAB(FIBLK)
            FTAB(FYC) = FTAB(FYC) - FTAB(FNX)
            END IF
C                                       single buffering?
         IF (IBUF.LE.0) THEN
            JBUF = 1
C                                       if double, flip buffer
         ELSE
            JBUF = 3 - IBUF
            FTAB(FIBUF) = JBUF
            END IF
 140     CALL ZWAIT (LUN, FIND, JBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         FTAB(FIND+12) = FTAB(FIND+12) - JBUF
         IF (IOP.EQ.IFINI) THEN
            JBUF = FTAB(FIND+12)
            IF ((JBUF.EQ.1) .OR. (JBUF.EQ.2)) CALL ZWAIT (LUN, FIND,
     *         JBUF, IERR)
            FTAB(FIND+12) = FTAB(FIND+12) - JBUF
            GO TO 999
         ELSE
            FTAB(FXC) = 1
            GO TO 20
            END IF
C                                       return pointer
 200  BIND = DBUF + FTAB(FIND+4) + (FTAB(FXC) - 1)*FTAB(FIND+5) +
     *   (FTAB(FIXC) - 1)*FTAB(FIND+15)
C                                       advance row counter
      IF (FTAB(FIXC).LT.FTAB(FIND+13)) THEN
         FTAB(FIXC) = FTAB(FIXC) + 1
      ELSE
         FTAB(FIXC) = 1
         FTAB(FXC) = FTAB(FXC) + 1
         END IF
C
 999  RETURN
      END
