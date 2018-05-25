      SUBROUTINE UVDISK (OP, LUN, FIND, BUFFER, NIO, BIND, IERR)
C-----------------------------------------------------------------------
C! reads/writes records of arbitrary length, esp UV data, see UVINIT
C# IO-basic UV-util
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
C   UVDISK reads and writes records of arbitrary length especially UV
C   visibility data.  Operation is faster if blocks of data are integral
C   numbers of disk blocks.  There are three operations which can be
C   invoked: READ, WRITE and FLUSH (OPcodes READ, WRIT and FLSH).
C
C   READ reads the next sequential block of data as specified to
C   UVINIT and returns the number of visibilities in NIO and the
C   pointer in BUFFER to the first word of this data.
C
C   WRIT arranges data in a buffer until it is full.  Then as many full
C   blocks as possible are written to the disk with the remainder left
C   for the next disk write.  For tape I/O data is always written with
C   the block size specified to UVINIT; one I/O operation per call.
C   For disk write, left-over data is transfered to the beginning of
C   buffer 1 if that is the next buffer to be filled.  Value of NIO in
C   the call is the number of vis. rec. to be added to the buffer and
C   may be fewer than the number speecified to UVINIT.  On return NIO
C   is the maximum number which may be sent next time.  On return BIND
C   is the pointer in BUFFER to begin filling new data.
C
C   FLSH writes integral numbers of blocks and moves any data left over
C   to the beginning of buffer 1.  One exception to this is when
C   NIO => -NIO or 0, in which case the entire remaining data in the
C   buffer is written.  After the call BIND is the pointer in BUFFER
C   for new data.  The principal difference between FLSH and WRIT is
C   that FLSH always forces an I/O transfer.  This may cause trouble if
C   a transfer of less than 1 block is requested.  A call with a
C   nonpositive value of NIO should be the last call and corresponds
C   to a call to MDISk with opcode 'FINI'.
C
C   NOTE:  A call to UVINIT is REQUIRED prior to calling UVDISK.
C
C   Inputs:
C      OP       C*4   Opcode 'READ','WRIT','FLSH' are legal
C      LUN      I     Logical unit number
C      FIND     I     FTAB pointer returned by ZOPEN
C      BUFFER   I(*)  Buffer for I/O
C      NIO      I     No. additional visibilities to write.
C   Output:
C      NIO      I     No. visibilities read.
C                     Max. no. vis. for next write.
C      BIND     I     Pointer to start of data in buffer
C      IERR     I     Return error code: 0 => OK
C                        1 => file not open in FTAB
C                        2 => input error
C                        3 => I/O error
C                        4 => end of file
C                        7 => attempt to write more vis than specified
C                             to UVINIT or will fit in buffer.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, BUFFER(*), NIO, BIND, IERR
C
      INTEGER   WRK, NVIS, BBO, XLEFT, VLEFT, ITEMP, FTAB6, NLEFT, NBR,
     *   IADDR, IBPT, INNIO, IOP, ISTART, JBUF, KBPS, KBUF, NBYTES
      LOGICAL   TAPE, FAST, FINISH
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Decode opcode.
      IOP = 0
      IF (OP.EQ.'READ') IOP = 1
      IF (OP.EQ.'WRIT') IOP = 2
      IF (OP.EQ.'FLSH') IOP = 3
C                                       Check for consistency
      IERR = 2
      IF ((IOP.NE.FTAB(FIND+9)) .AND. (IOP.NE.3)) GO TO 999
C                                       Check if open.
      IERR = 1
      IF (FTAB(FIND).NE.LUN) GO TO 999
      IERR = 0
C                                       Store input NIO
      INNIO = NIO
C                                       Check if read finished,
C                                       if so return NIO=0.
      NVIS = FTAB(FIND+2)
      BBO = FTAB(FIND+4)
      IF ((IOP.NE.1) .OR. (NVIS.GT.0)) GO TO 5
         IF (FTAB(FIND+13).GE.FTAB(FIND+12)) NIO = 0
         IF (FTAB(FIND+13).GE.FTAB(FIND+12)) GO TO 999
         GO TO 10
C                                       Check if more writes than
C                                       legal.
 5    IF (IOP.EQ.1) GO TO 10
         IF ((ABS(NIO).GT.(FTAB(FIND+12)-FTAB(FIND+13)))) IERR = 7
         IF ((NVIS.LE.0) .AND. (FTAB(FIND+13).GE.FTAB(FIND+12)))
     *      IERR = 7
         IF (IERR.NE.0) GO TO 999
C                                       For write, incr. # in buff.
         FTAB(FIND+13) = FTAB(FIND+13) + ABS(NIO)
C                                       Check if time for I/O.
 10   IF ((FTAB(FIND+13).GE.FTAB(FIND+12)) .OR. (IOP.EQ.3)) GO TO 15
C                                       Update pointers and return.
C                                       Determine number
C                                       of vis this call.
         NIO = MIN (FTAB(FIND+12)-FTAB(FIND+13), FTAB(FIND+11))
         IF (IOP.EQ.1) INNIO = FTAB(FIND+11)
C                                       # Processed in buffer. READ
         IF (IOP.EQ.1) FTAB(FIND+13) = FTAB(FIND+13) + ABS (NIO)
C                                       Set buffer call pointer.
         BIND = FTAB(FIND+15) + INNIO * FTAB(FIND+10)
         FTAB(FIND+15) = BIND
         GO TO 999
C                                       It's I/O time again folks.
C
 15   TAPE = DEVTAB(LUN).NE.0
C                                       JBUF = current buffer no.
      JBUF = ABS (FTAB(FIND+8))
C                                       Check if an intergral # of
C                                       blocks in this transfer.
      FAST = (MOD (FTAB(FIND+1),NBPS).EQ.0) .AND. (FTAB(FIND+6).EQ.0)
C                                       Determine next buffer #.
      KBUF = 1
      IF (FTAB(FIND+8).GT.0) KBUF = 3 - JBUF
C                                       If OP='FLSH' next buffer is
C                                       forced to to be 1
      IF (IOP.EQ.3) KBUF = 1
C                                       Determine # vis left to do.
      WRK = FTAB(FIND+13)
      NVIS = NVIS - WRK
      FTAB(FIND+2) = NVIS
      VLEFT = NVIS
C                                       Check if # vis completed.
C                                       READ
      IF (IOP.GE.2) GO TO 100
C                                       Determine number of bytes for
C                                       I/O.  If double buffering,
C                                       subtract an extra I/O from
C                                       apparent visibility count.
C                                       VLEFT => this call
C                                       XLEFT => next call.
         XLEFT = VLEFT
         IF (FTAB(FIND+8).GT.0) XLEFT = VLEFT - FTAB(FIND+12)
         NBYTES = FTAB(FIND+13) * FTAB(FIND+10) * 2 +
     *      FTAB(FIND+6)
         IF (XLEFT.GE.0) GO TO 20
C                                       Set max # vis in buffer
            IF (VLEFT.LT.0) FTAB(FIND+12) = FTAB(FIND+12) + VLEFT
C                                       Last READ single buffering.
C                                       Update NBYTES
            NBYTES = NBYTES + (XLEFT * FTAB(FIND+10) * 2)
            NBYTES = MAX (NBYTES, NBPS)
C                                        If this is the last double
C                                        buffer call skip read.
 20      IF ((VLEFT.LE.0) .AND. (FTAB(FIND+8).GT.0)) GO TO 30
C                                       Set NBYTES for tape
            IF (TAPE) NBYTES = FTAB(FIND+1)
C                                       Determine buffer pointer
C                                       for next I/O
            IBPT = (JBUF-1) * FTAB(FIND+1)/2 + 1
C                                       Do next I/O
            CALL ZMIO ('READ', LUN, FIND, BBO, NBYTES, BUFFER(IBPT),
     *         JBUF, IERR)
C                                       Update FTAB
            WRK = (FTAB(FIND+1)+FTAB(FIND+6)) / NBPS - 1
            IF ((FAST) .OR. (TAPE)) WRK = WRK + 1
C                                       Update block count for
C                                       next I/O
            BBO = BBO + WRK
            FTAB(FIND+4) = BBO
C                                       Set NIO for output.
 30      NIO = MIN (FTAB(FIND+12), FTAB(FIND+11))
C                                       Reset count in buffer.
         FTAB(FIND+13) = NIO
C                                       Save buffer number.
         IF (FTAB(FIND+8).GT.0) FTAB(FIND+8) = KBUF
C                                       Compute BIND
C                                       If double buffer back byte off
         FTAB6 = FTAB(FIND+6)
         IF (FTAB(FIND+8).GT.0) FTAB6 = NBPS -
     *      MOD (FTAB(FIND+1)-FTAB6, NBPS)
         FTAB6 = MOD (FTAB6, NBPS)
         IF ((FAST) .OR. (TAPE)) FTAB6 = 0
         BIND = ((KBUF-1) * FTAB(FIND+1) + FTAB6) / 2 + 1
C                                       First byte of next I/O
         FTAB(FIND+6) = MOD (FTAB(FIND+6)+FTAB(FIND+1), NBPS)
         IF ((TAPE) .OR. (FAST)) FTAB(FIND+6) = 0
C                                       Buffer pointer for next call
         FTAB(FIND+15) = BIND
C                                       Wait for previous I/O
         CALL ZWAIT (LUN, FIND, KBUF, IERR)
         GO TO 999
C
C                                       Write.
 100  CONTINUE
C                                       Check to see if a request was
C                                       made to terminate this file and
C                                       completely flush the buffer.
         FINISH = (IOP.EQ.3) .AND. (INNIO.LE.0)
C                                       Determine location in buffer
C                                       to begin transfer
         IBPT = MAX (FTAB(FIND+14), 1)
C                                       NBR = # bytes ready to write.
         NBR = 2 * FTAB(FIND+13) * FTAB(FIND+10) + FTAB(FIND+6)
         NBYTES = NBR - MOD (NBR, NBPS)
         NBYTES = MAX (NBYTES, NBPS)
C                                       Do write
C                                       If FINISH do whole thing
         IF ((FINISH) .OR. (VLEFT.LE.0)) NBYTES = MAX (NBR, NBPS)
         IF (TAPE) NBYTES = FTAB(FIND+1)
C                                        Check for null write.
         IF (NBR.LE.0) NBYTES = 0
         CALL ZMIO ('WRIT', LUN, FIND, BBO, NBYTES, BUFFER(IBPT),
     *      JBUF, IERR)
C                                       Set new byte offset.
         FTAB(FIND+6) = NBR - NBYTES
         IF ((TAPE) .OR. (FAST)) FTAB(FIND+6) = 0
C                                       Update this-write buffer
C                                       pointer, allow space
C                                       for left over data.
         KBPS = 2 * NBPS
         IF (FAST) KBPS = 0
         FTAB(FIND+14) = FTAB(FIND+14) + (NBR + KBPS) / 2
         IF (KBUF.EQ.1) FTAB(FIND+14) = 1
C                                       Update this-call buffer
C                                       pointer.
         BIND = FTAB(FIND+14) + FTAB(FIND+6) / 2
         FTAB(FIND+15) = BIND
         FTAB(FIND+13) = 0
C                                       Set output NIO
         NLEFT = FTAB(FIND+12)
         IF (VLEFT.LE.0) NLEFT = NLEFT + VLEFT
         NLEFT = MAX (0, NLEFT)
         NIO = MIN (FTAB(FIND+11), NLEFT)
C                                       If about to finish NVIS
C                                       update FTAB(FIND+12)
         FTAB(FIND+12) = NLEFT
C                                       Update block count for next I/O
         BBO = BBO + NBYTES / NBPS
         FTAB(FIND+4) = BBO
C                                       Update buffer no.
         IF (FTAB(FIND+8).GT.0) FTAB(FIND+8) = KBUF
C                                       If finished mark vis count
         IF ((FINISH) .OR. (VLEFT.LE.0)) NIO = 0
C                                       Wait for current buffer.
         CALL ZWAIT (LUN, FIND, KBUF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy any unwriten values from
C                                       the end of the last buffer
C                                       to the start next buffer.
         IF (FTAB(FIND+6).LE.0) GO TO 999
            ISTART = IBPT + NBYTES/2
            IADDR = MAX (FTAB(FIND+14), 1)
            ITEMP = FTAB(FIND+6) / 2
            CALL COPY (ITEMP, BUFFER(ISTART), BUFFER(IADDR))
C
 999  RETURN
      END
