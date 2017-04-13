      SUBROUTINE MINIT (OP, LUN, IND, LX, LY, WIN, BUFF, BFSZ, BLKOF,
     *   IERR)
C-----------------------------------------------------------------------
C! intializes IO and pointers for quick-return image IO via MDISK
C# IO-basic Map-util
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
C   MINIT sets up a special section of FTAB for quick-return, double
C   buffered I/O.  N.B. This routine is designed to read/write images
C   one plane at a time.  One can run the planes together iff the rows
C   are not blocked: i.e. iff NBPS / (LX * 2) < 2.
C   Inputs:
C      OP    C*4   Operation code character string: 'READ','WRIT','UPDT'
C      LUN   I     logical unit number
C      IND   I     pointer to FTAB, returned by ZOPEN
C      LX    I     Number of pixels per line in X-direction for whole
C                  map
C      LY    I     Number of lines in whole plane
C      WIN   I(4)  Xmin,Ymin,Xmax,Ymax defining desired subrectangle in
C                  the current plane
C      BFSZ  I     Size of total available buffer in AIPS bytes
C      BLKOF I     block number, 1 relative, of first map pixel in this
C                  plane of the image
C   Outputs:
C      IERR  I     Error return: 0 => ok
C                             1 -> file not open
C                             2 => input error
C                             7 => Buffer too small
C                             3 => i/o error on initialize
C                             4 => end of file
C                             5 => beginning of medium
C                             6 => end of medium
C   Usage notes: For map i/o the first 16 words in each FTAB entry
C   contain a user table to handle double buffer i/o,  the rest
C   contain system-dependent IO tables.  A "major line" is 1 row or
C   1 sector if more than 1 line fits in a sector.  FTAB user table
C   entries, with offsets from the FIND pointer are:
C      FTAB + 0 =>  LUN using this entry
C             1 =>  No. of major lines transfered per i/o op
C             2 =>  No. of major times a buffer has been acessed
C             3 =>  No. of major lines remaining on disk
C             4 =>  Output index for first pixel in window
C             5 =>  No. pixels to increment for next major line
C             6 =>  Which buffer to use for i/o; -1 => single buffer
C             7 =>  Block offset in file for next operation
C             8 =>
C             9 =>  Block increment in file for each operation
C            10 =>  No. of bytes transferred
C            11 =>  I/O op code
C            12 =>  sum of any buffer numbers needing to be waited upon
C            13 =>  # rows / major line (>= 1)
C            14 =>  # times this major line has been accessed
C            15 =>  # pixels to increment for next row (= LX)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, IND, LX, LY, WIN(4), BFSZ, BLKOF, IERR
      REAL      BUFF(*)
C
      LOGICAL   BACK, TAPE
      INTEGER   IOP, NBYTE, JBUF, BLKLX, BLKW1, BLKW3, FBBLK, FIBLK,
     *   DBUF, BYT0, NBLK0, NEX, IBUF, FNX, FYC, IWRITE, IREAD, BFSZ2,
     *   W(4), FUNF, I, ILX, IS2, IS4, MBYTE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IWRITE, IREAD /1,2/
C-----------------------------------------------------------------------
C                                       Test OPcode
      IERR = 2
      IF (MOD(DEVTAB(LUN),2).EQ.1) GO TO 999
      IOP = 0
      IF (OP.EQ.'WRIT') IOP = IWRITE
      IF (OP.EQ.'READ') IOP = IREAD
      IF (IOP.NE.0) GO TO 10
         WRITE (MSGTXT,1000) OP
         GO TO 980
C                                       Check for Default WINDOW
 10   IF (WIN(1).LE.0) THEN
         W(1) = 1
         W(2) = 1
         W(3) = LX
         W(4) = LY
         BACK = .FALSE.
      ELSE
         DO 20 I = 1,4
            W(I) = WIN(I)
 20         CONTINUE
C                                       Check for backward read
         BACK = W(2).GT.W(4)
         IF (BACK) THEN
            I = W(4)
            W(4) = W(2)
            W(2) = I
            END IF
C                                       No X-WINDOW allowed on WRITE
         IF ((IOP.EQ.IWRITE) .AND. ((W(1).NE.1) .OR. (W(3).NE.LX)))
     *      GO TO 970
C                                       Check if WINDOW is in bounds
         IF ((W(1).LE.0) .OR. (W(3).GT.LX)) GO TO 970
         IF ((W(2).LE.0) .OR. (W(4).GT.LY)) GO TO 970
         END IF
C                                       Check if file in FTAB
 30   IERR = 1
      IF (LUN.EQ.FTAB(IND)) GO TO 35
          WRITE (MSGTXT,1030) LUN
          GO TO 980
C                                       Is this a tape unit?
C                                       or other non-disk unit
 35   TAPE = (DEVTAB(LUN).NE.0)
      ILX = NBPS / (2 * LX)
      ILX = MAX (ILX, 1)
      IF (TAPE) ILX = 1
      IF ((IOP.EQ.IWRITE) .AND. (MOD(W(2)-1,ILX).NE.0)) GO TO 970
      IF ((IOP.EQ.IWRITE) .AND. (MOD(W(4),ILX).NE.0) .AND.
     *   (W(4).NE.LY)) GO TO 970
C                                       Set up table
      BLKLX = 1 + (2*LX - 1) / NBPS
      BLKW3 = (2*W(3) - 1) / NBPS
      BLKW1 = (2*W(1) - 1) / NBPS
C                                       No. of blocks and bytes to read
      NBLK0 = 1 + BLKW3 - BLKW1
      BYT0  = NBLK0 * NBPS
C                                       No blocking for tapes
      IF (TAPE) BYT0 = LX * 2
C                                       Is buffer large enough
      IERR = 7
      IF (BYT0.LE.BFSZ) GO TO 40
         WRITE (MSGTXT,1035) LX, BFSZ
         GO TO 980
C                                       Can we double buffer
 40   IERR = 0
      IBUF = -1
      NEX = 0
      IS4 = (W(4)-1) / ILX + 1
      IS2 = (W(2)-1) / ILX + 1
      BFSZ2 = BFSZ / 2
C                                      Double buffer
      IF (BYT0.LE.BFSZ2) THEN
         IBUF = 1
C                                       more than 1 line in buffer?
         NEX = (BFSZ2 - BYT0) / (BLKLX * NBPS)
         NEX = MIN (NEX, IS4-IS2)
         END IF
C                                       Fill in table
      FNX   = IND + 1
      FYC   = IND + 3
      FBBLK = IND + 7
      FIBLK = IND + 9
      FTAB(FNX)      = 1 + NEX
      FTAB(IND + 2)  = 1
      FTAB(FYC)      = 1 + IS4 - IS2
      FTAB(IND + 4)  = W(1) - ((BLKW1*NBPS)/2)
      FTAB(IND + 5)  = (BLKLX*NBPS)/2
      FTAB(IND + 6)  = IBUF
      FTAB(IND +12)  = 0
      FTAB(IND +13)  = ILX
      FTAB(IND +14)  = MOD (W(2)-1, ILX) + 1
      FTAB(IND +15)  = LX
C                                        Do some math to calcul
      FTAB(FBBLK) = BLKLX * (IS2 - 1) + BLKW1 + BLKOF
      FTAB(FIBLK) =  BLKLX
      NBYTE = BYT0 + NEX * NBPS * BLKLX
      IF (TAPE) NBYTE = BYT0
      FTAB(IND + 10)  = NBYTE
      FTAB(IND + 11) = IOP
C                                        Modify for backward reading
      IF (BACK) THEN
         FTAB(IND + 4) = FTAB(IND + 4) + NEX * FTAB(IND + 5) +
     *      (ILX - 1) * LX
         FTAB(IND +15) = -LX
         FTAB(IND +14) = ILX - MOD (W(4)-1, ILX)
         FTAB(IND + 5) = - FTAB(IND + 5)
         FTAB(FIBLK) =   - BLKLX
         FTAB(FBBLK) = BLKLX * (IS4 - 1 - NEX) + BLKW1 + BLKOF
         END IF
C                                       Modify for tapes-unblocked
      IF (TAPE) THEN
         FTAB(FNX) = 1
         FTAB(IND + 4) = W(1)
         END IF
C                                       If WRITE issue a dummy write to
C                                       buffer 2
      IF (IOP.EQ.IWRITE) THEN
         JBUF = 2
         MBYTE = 0
         IF (IBUF.EQ.-1) GO TO 999
         CALL ZMIO ('WRIT', LUN, IND, FTAB(FBBLK), MBYTE, BUFF, JBUF,
     *      IERR)
         FTAB(IND+12)  = FTAB(IND+12) + JBUF
C                                       Start first read
      ELSE
         JBUF = 1
         CALL ZMIO ('READ', LUN, IND, FTAB(FBBLK), NBYTE, BUFF, JBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         FTAB(IND+12)  = FTAB(IND+12) + JBUF
C                                       Update block counter
         FTAB(FBBLK) = FTAB(FBBLK) + FTAB(FIBLK) * (1 + NEX)
         FTAB(FYC) = FTAB(FYC) - FTAB(FNX)
C                                       Start 2nd read
         IF ((IBUF.GE.0) .AND. (FTAB(FYC).GT.0)) THEN
            DBUF = 2 + (NBYTE - 1) / 2
            JBUF = 2
C                                       Is this the last read
            FUNF = FTAB(FNX) - FTAB(FYC)
            IF (FUNF.GT.0) THEN
C                                       Yes: forward
               IF (FTAB(FIBLK).GE.0) THEN
                  NBYTE = NBYTE - FUNF*FTAB(FIBLK)*NBPS
C                                       Yes: backward
               ELSE
                  NBYTE = NBYTE + FUNF * FTAB(FIBLK) * NBPS
                  DBUF = DBUF - (FUNF * NBPS * FTAB(FIBLK)) / 2
                  FTAB(FBBLK) = FTAB(FBBLK) - FTAB(FIBLK) * FUNF
                  END IF
               END IF
            CALL ZMIO ('READ', LUN, IND, FTAB(FBBLK), NBYTE, BUFF(DBUF),
     *         JBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            FTAB(IND+12)  = FTAB(IND+12) + JBUF
            FTAB(FBBLK) = FTAB(FBBLK) + FTAB(FIBLK) * (1 + NEX)
            FTAB(FYC) = FTAB(FYC) - FTAB(FNX)
            END IF
C                                       Wait for 1st buffer
         JBUF = 1
         CALL ZWAIT (LUN, IND, JBUF, IERR)
         FTAB(IND+12)  = FTAB(IND+12) - JBUF
         END IF
      GO TO 999
C                                       error messages
 970  WRITE (MSGTXT,1970) OP, WIN, LX, LY
 980  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MINIT: OPCODE ',A4,' INVALID')
 1030 FORMAT ('MINIT: FILE FOR LUN',I4,' NOT OPEN')
 1035 FORMAT ('MINIT: BUFFER TOO SMALL LX =',I5,' BY/PX=2   BUFSZ=',I6)
 1970 FORMAT ('MINIT: OP ',A4,' ERROR IN WIN,LX,LY=',6I6)
      END
