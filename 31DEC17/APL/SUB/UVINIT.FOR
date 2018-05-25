      SUBROUTINE UVINIT (OP, LUN, FIND, NVIS, VISOFF, LREC, NPIO, BUFSZ,
     *   BUFFER, BO, BIND, IERR)
C-----------------------------------------------------------------------
C! initializes IO for arbitrary length records via UVDISK, esp UV data
C# IO-basic UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2015
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
C   UVINIT sets up bookkeeping for the UV data I/O routine UVDISK.  I/O
C   for these routines is double buffered (if possible) quick return
C   I/O.  UVDISK will run much more efficiently if on disk LREC*NPIO
C   is an integral number of blocks.  Otherwise, partial writes or
C   oversize reads will have to be done.  Minimum disk I/O is one
C   block.  Smaller calls to UVINIT may be made as long as the buffer
C   is large enough (double buffer req).  The buffer size should include
C   an extra NBPS bytes for each buffer for  read if NPIO
C   records does not correspond to an integral number of disk sectors
C   (NBPS bytes).  2*NBPS extra bytes required for each buffer for
C   write.
C        NPIO will be adjusted to the maximum allowed for double
C   buffering if the input value is .LE. 0, or the maximum allowed
C   single buffering value if NPIO is too large.  If it is positive and
C   useable it is used.
C
C   Inputs:
C      OP       C*4   OP code, 'READ' or 'WRIT' for desired operation.
C      LUN      I     Logical unit number of file.
C      FIND     I     FTAB pointer for file returned by ZOPEN.
C      NVIS     I     Number of visibilities to be transfered.
C      VISOFF   I     Offset in vis. rec. of first vis. rec. from BO.
C      LREC     I     Number of values in a visibility record.
C      NPIO     I     Number of visibilities per call to UVDISK.
C                     Determines block size for tape I/O
C                     0 => decide (see note above)
C      BUFSZ    I     Size in bytes of the buffer.
C      BUFFER   R(*)  Buffer
C      BO       I     Block offset to begin transfer from (1-relative)
C   Output:
C      NPIO     I     The max. number of visibilities which can be
C                     be written or will be read per call.
C      BIND     I     Pointer in BUFFER for WRITE operations.
C      IERR     I     Return error code:
C                     0 => OK
C                     1 => file not open in FTAB
C                     2 => invalid input parameter.
C                     3 => I/O error
C                     4 => End of file.
C                     7 => buffer too small
C   Note: VISOFF and BO are additive.
C   UVINIT sets and UVDISK uses values in the FTAB:
C      FTAB(FIND+0) = LUN
C                1  = # Bytes per I/O
C                2  = # vis. records left to transfer. For double buffer
C                     read, 1 more I/O will have been done than shown
C                3  =
C                4  = Block offset for next I/O.
C                5  =
C                6  = byte offset of next I/O
C                7  =
C                8  = Current buffer #, -1 => single buffering
C                9  = OPcode 1 = read, 2 = write.
C               10  = Values per visibility record.
C               11  = # vis. records per UVDISK call
C               12  = max. # vis. per buffer.
C               13  = # vis. processed in this buffer.
C               14  = Buffer pointer for start of current buffer
C                     (in values).  Used for WRIT only; includes any
C                     data carried over from the last write.
C               15  = Buffer pointer for call (values)
C   Programmer: W. D. Cotton, Feb. 1981.  (Rev EWG Aug 1984)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, NVIS, VISOFF, LREC, NPIO, BUFSZ, BO, BIND,
     *   IERR
      REAL      BUFFER(*)
C
      INTEGER   WRK, BBO, WRK2, WRK3, LBPIO, NRPB, BYOFF, I, IBPT, KBPS,
     *   LENIO, JBPS, NBPIO, NBUF, NBYTES, NCPB
      DOUBLE PRECISION  DWRK, DWRK2
      LOGICAL   FAST
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check input parms
      IERR = 2
      IF ((LREC.LE.0) .OR. (NVIS.LT.0) .OR. (BUFSZ.LE.0) .OR. (BO.LE.0)
     *   .OR. ((NVIS.EQ.0) .AND. (OP.NE.'READ'))) THEN
         WRITE (MSGTXT,1000) LREC, NPIO
         CALL MSGWRT (8)
         WRITE (MSGTXT,1001) NVIS, BUFSZ, BO
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Check if file open.
      IERR = 1
      IF (FTAB(FIND).NE.LUN) GO TO 999
C                                       Check if default NPIO - if so
C                                       Set to large double buffer size.
C                                       (assuming .NOT.FAST)
      JBPS = NBPS
      IF (OP.EQ.'WRIT') JBPS = 2 * JBPS
      IF (NPIO.LE.0) NPIO = (BUFSZ-2*JBPS) / (LREC * 4)
C                                       Single buffer if necessary
      IF (NPIO.LE.0) NPIO = (BUFSZ-JBPS) / (LREC * 2)
C                                       Compute bloc and byte offsets
C                                       corresponding to VISOFF
      BYOFF = 0
      BBO = BO
      IF (VISOFF.NE.0) THEN
         DWRK = (2.0D0 * LREC) * VISOFF
         WRK2 = DWRK / NBPS
         DWRK2 = WRK2
C                                       Update block offset.
         BBO = BBO + WRK2
C                                       Compute byte offset.
         DWRK2 = DWRK2 * NBPS
         BYOFF = DWRK - DWRK2 + 0.1D0
         END IF
C                                       Make sure NPIO.LE.NVIS
      IF (NPIO.GT.NVIS) NPIO = MAX (1, NVIS)
C                                       Check if I/O in integral mult.
C                                       of blocks.  If so FAST=true
C                                       and KBPS=0; otherwise FAST=
C                                       false and KBPS=NBPS to allow
C                                       for excess length I/O.
      KBPS = NBPS
C                                       Come back to here if NPIO too
C                                       big.
 15   LENIO = 2 * NPIO * LREC
      FAST = (MOD(LENIO,NBPS).EQ.0) .AND. (BYOFF.EQ.0)
      IF (FAST) KBPS = 0
C                                       For write KBPS = 2*KBPS
      IF (OP.EQ.'WRIT') KBPS = 2 * KBPS
C                                       Tape I/O are always
C                                       NPIO vis. records.
C                                       LBPIO = max # bytes per
C                                       I/O including effects of
C                                       not starting on bloc boundary
      LBPIO = LENIO + KBPS
C                                       If NPIO too big decrease
      IF (LBPIO.GT.BUFSZ) THEN
C                                       Biggest single buffer
         NPIO = (BUFSZ - JBPS) / (2 * LREC)
         KBPS = NBPS
C                                       Loop back and refigure
         GO TO 15
         END IF
C                                       Determine no. of buffers,
C                                       double buffer if possible.
      NBUF = 2
      IF (LBPIO*2.GT.BUFSZ) NBUF = 1
C                                       Make sure buffer big enough
      IERR = 7
      IF ((LBPIO.GT.BUFSZ) .OR. (NPIO.LT.1)) THEN
         WRITE (MSGTXT,1010) BUFSZ, LREC
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine # calls per buffer
C                                       for tape = 1, for disks allow
C                                       NBPS extra room if nec.
      NCPB = ((BUFSZ - NBUF*KBPS) / NBUF) / LENIO
      NBPIO = NCPB * LENIO + KBPS
C                                        Check again if fast.
      FAST = FAST .OR. ((MOD(NBPIO-KBPS,NBPS).EQ.0) .AND. (BYOFF.EQ.0))
      IF (FAST) KBPS = 0
      NBPIO = NCPB * LENIO + KBPS
C                                       Check min. disk write size.
      NRPB = NPIO
      IF ((NCPB*LENIO.LT.NBPS) .AND. (OP.EQ.'WRIT') .AND.
     *   (NRPB.LT.NVIS))  GO TO 999
      IERR = 0
C                                       Load FTAB
      FTAB(FIND+1) = NBPIO
      FTAB(FIND+2) = NVIS
      FTAB(FIND+4) = BBO
      FTAB(FIND+6) = BYOFF
      FTAB(FIND+8) = 2 * NBUF - 3
      FTAB(FIND+10) = LREC
      FTAB(FIND+11) = NPIO
      FTAB(FIND+12) = NCPB * NPIO
      FTAB(FIND+13) = 0
      FTAB(FIND+14) = 1
      FTAB(FIND+15) = 1
C                                       Determine operation.
      FTAB(FIND+9) = 0
      IF (OP.EQ.'READ') FTAB(FIND+9) = 1
      IF (OP.EQ.'WRIT') FTAB(FIND+9) = 2
C                                       Unknown opcode
      IF (FTAB(FIND+9).GT.0) GO TO 30
         WRITE (MSGTXT,1020) OP
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
C                                       Do first I/O operation.
C                                       Read.
 30   IF (FTAB(FIND+9).EQ.2) GO TO 100
C                                       Save original vis count.
         WRK2 = NVIS
C                                       Dec. vis. count for next buffer
         WRK3 = NVIS - FTAB(FIND+12)
         FTAB(FIND+2) = WRK3
C                                       Set buffer pointer.
         FTAB(FIND+15) = 1 + BYOFF/2
         DO 50 I = 1,NBUF
C                                       Set number of vis left
C                                       after first read and determine
C                                       number of bytes to read.
            WRK2 = WRK2 - FTAB(FIND+12)
            NBYTES = FTAB(FIND+12) * FTAB(FIND+10) * 2 + FTAB(FIND+6)
            IF (WRK2.LT.0) NBYTES = NBYTES + (2*WRK2*LREC)
            NBYTES = MAX (NBYTES, NBPS)
C                                       Determine buffer pointer.
            IBPT = (I-1) * FTAB(FIND+1)/2 + 1
C                                       Do read.
            CALL ZMIO ('READ', LUN, FIND, BBO, NBYTES, BUFFER(IBPT),
     *         I, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Update FTAB
            WRK = (FTAB(FIND+1) + FTAB(FIND+6)) / NBPS - 1
            IF (FAST) WRK = WRK + 1
            BBO = BBO + WRK
            FTAB(FIND+6) = MOD ((FTAB(FIND+6)+FTAB(FIND+1)), NBPS)
            IF (FAST) FTAB(FIND+6) = 0
C                                       Check if first I/O was
C                                       the last one.
            IF (WRK2.LE.0) GO TO 60
 50         CONTINUE
C                                       Set buffer pointers.
C                                       Check if finished.
C                                       If finished ON first read
C                                       set tot. # in buffer.
 60      IF ((WRK3.LT.0) .AND. (I.EQ.1)) FTAB(FIND+12) =
     *      FTAB(FIND+12) + WRK3
         FTAB(FIND+15) = FTAB(FIND+15) - MIN (FTAB(FIND+11),
     *      FTAB(FIND+12)) * LREC
C                                       Wait for 1st buffer.
         FTAB(FIND+4) = BBO
         CALL ZWAIT (LUN, FIND, 1, IERR)
         GO TO 999
C                                       Dummy 1st write to buffer
C                                       if double buffering.
C                                       Allow starting in middle of file
 100  IF (VISOFF.LE.0) THEN
         BIND = 1
         IF (NBUF.EQ.1) GO TO 999
            NBYTES = 0
            CALL ZMIO ('WRIT', LUN, FIND, BBO, NBYTES, BUFFER, 2,
     *         IERR)
C                                       Read previous data
      ELSE
C                                       Set buffer pointer.
         FTAB(FIND+15) = 1 + BYOFF/2
         BIND = FTAB(FIND+15)
C                                       number of bytes to read.
         NBYTES = BYOFF
C                                       Do read.
         CALL ZMIO ('READ', LUN, FIND, BBO, NBYTES, BUFFER, 1, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Wait
         CALL ZWAIT (LUN, FIND, 1, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Dummy write if necessary
         IF (NBUF.GT.1) THEN
            NBYTES = 0
            CALL ZMIO ('WRIT', LUN, FIND, BBO, NBYTES, BUFFER, 2, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVINIT: LREC, NPIO =',2I7,' ILLEGAL')
 1001 FORMAT ('UVINIT: OR NVIS, BUFSZ, OR BO =',I10,2I7,' ILLEGAL')
 1010 FORMAT ('UVINIT: BUFSZ=',I10,' TOO SMALL, LREC=',I5)
 1020 FORMAT ('UVINIT: ILLEGAL OPCODE=',A4)
      END
