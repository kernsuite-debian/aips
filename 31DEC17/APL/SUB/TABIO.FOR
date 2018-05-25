      SUBROUTINE TABIO (OPCODE, IRCODE, IRNO, RECORD, BUFFER, IERR)
C-----------------------------------------------------------------------
C! reads/writes tables extension files
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2012
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
C   TABIO does random access I/O to Tables extension files.  Mixed
C   reads and writes are allowed if TABINI was called 'WRIT'.  Writes
C   are limited by the size of the structure (i.e. # columns for units
C   and titles) or to the current maximum logical record plus one.
C   Files opened for WRITe are updated and compressed on CLOS.
C   Inputs:
C      OPCODE   C*4      Opcode 'READ','CLOS'
C                           'WRIT' : write data as selected
C                           'FLAG' : write data as de-selected
C      IRCODE   I        Type of information
C                           0 => Table row
C                           1 => DATPTR/DATYPE record
C                           2 => data selection string
C                           3 => title
C                           4 => units
C                           5 => keyword/value pair
C      IRNO     I        Logical record number. 0 => next (can work
C                        with row data and latest IRCODE > 0 only)
C                           IRNO is row number (IRCODE = 0)
C                           IRNO is ignored (IRCODE = 1)
C                           IRNO is string number (IRCODE = 2)
C                           IRNO is column number (IRCODE = 3)
C                           IRNO is column number (IRCODE = 4)
C                           IRNO is keyword number (IRCODE = 5)
C      RECORD   I(*)     Array containing record to be written
C      BUFFER   I(*)     Work buffer = 256 word + enough 256 word
C                        blocks for at least one full logical record.
C                        Must be the same one given TABINI.
C   Output:
C      RECORD   I(*)     Array containing record read.
C      BUFFER   I(*)     buffer.
C      IERR     I        Return error code 0 => OK
C                            -1 => on READ: row read is flagged
C                             1 => file not open
C                             2 => input error
C                             3 => I/O error
C                             4 => attempt to read past end of data
C                                  or write past end of data + 1
C                             5 => error on expanding the file
C   IMPORTANT NOTE:  the contents of BUFFER should not be changed
C   except by TABIO between the time TABINI is called until the file
C   is closed.  The exception is that the user portion of the header
C   record is available.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   IRCODE, IRNO, RECORD(*), BUFFER(*), IERR
C
      INTEGER   TRNO, LRNO, LLRNO, MRNO, XRNO, ISIZE, IOP, IADD, NLPR,
     *   IOFF, I, LIMIT, INDEX, IER, NWORD, SCRTCH(256)
      CHARACTER PHNAME*48, CTEST*4, OP*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Test OP, record codes
      IERR = 2
      IF ((IRCODE.LT.0) .OR. (IRCODE.GT.5)) GO TO 970
      IOP = 0
      IF (OPCODE.EQ.'CLOS') GO TO 200
      IF (OPCODE.EQ.'READ') IOP = 1
      IF (OPCODE.EQ.'WRIT') IOP = 2
      IF (OPCODE.EQ.'FLAG') IOP = 3
      IF (IOP.EQ.0) GO TO 970
      IF ((IOP.GT.1) .AND. (BUFFER(71).LE.1)) GO TO 970
C                                       Get logical record number
      TRNO = BUFFER(75)
      LRNO = IRNO - 1
      IF ((LRNO.LT.0) .AND. (IRCODE.EQ.0)) LRNO = TRNO
      IF ((LRNO.LT.0) .AND. (IRCODE.EQ.BUFFER(80))) LRNO = BUFFER(79)
      LRNO = LRNO + 1
      LLRNO = LRNO
C                                       logical -> physical col #
      IF ((IRCODE.EQ.3) .OR. (IRCODE.EQ.4)) LRNO = BUFFER(128+LRNO)
      MRNO = 1
      IF ((LRNO.LE.0) .AND. (IRCODE.NE.1)) GO TO 980
C                                       Get maximum allowed rec #
      IADD = 0
      IF (IOP.GT.1) IADD = 1
      MRNO = BUFFER(10)
      IF (IRCODE.EQ.0) MRNO = BUFFER(5) + IADD
      IF (IRCODE.EQ.2) MRNO = MIN (BUFFER(61) + IADD, 8)
      IF (IRCODE.EQ.5) MRNO = MIN (BUFFER(53) + IADD, BUFFER(52))
      IF ((LRNO.GT.MRNO) .AND. (IRCODE.NE.1)) GO TO 980
C                                       Set physical record #, offset
      IF (IRCODE.EQ.0) THEN
         NLPR = BUFFER(9)
         IF (NLPR.GT.0) THEN
            MRNO = BUFFER(50) + (LRNO - 1) / NLPR
            IOFF = (LRNO - 1) - (MRNO - BUFFER(50)) * NLPR
            IOFF = IOFF * BUFFER(72) + 1
         ELSE
            MRNO = BUFFER(50) - (LRNO - 1) * NLPR
            IOFF = 1
            END IF
      ELSE IF (IRCODE.EQ.1) THEN
         MRNO = 2
         IOFF = 1
      ELSE IF (IRCODE.EQ.2) THEN
         NLPR = BUFFER(62)
         IF (LRNO.LE.BUFFER(61)) NLPR = BUFFER(62+LRNO)
         MRNO = BUFFER(46)
         IOFF = NLPR
         IF (IOFF.GT.256) THEN
            MRNO = MRNO + 1
            IOFF = IOFF - 256
            END IF
      ELSE
         I = 6
         IF (IRCODE.EQ.4) I = 2
         IF (IRCODE.EQ.5) I = 5
         NLPR = 256 / I
         MRNO = BUFFER(44+IRCODE) + (LRNO - 1) / NLPR
         IOFF = (LRNO - 1) - ((LRNO-1)/NLPR) * NLPR
         IOFF = IOFF * I + 1
         END IF
C                                       Large row - immediate I/O
      IF ((IRCODE.EQ.0) .AND. (BUFFER(9).LT.0)) THEN
         LIMIT = -BUFFER(9)
         IERR = 0
C                                       File open for write; expand
C                                       if necessary.
         IF (BUFFER(71).EQ.2) THEN
C                                       expand file
            IF ((MRNO+LIMIT-1.GT.BUFFER(1)) .OR. (LRNO.GT.BUFFER(3)))
     *         THEN
               IADD = LIMIT * BUFFER(42)
               CALL H2CHR (48, 1, BUFFER(17), PHNAME)
               CALL ZEXPND (BUFFER(81), BUFFER(32), PHNAME, IADD, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1050) IER
                  IERR = 5
                  GO TO 995
                  END IF
C                                       Update header info
               BUFFER(1) = BUFFER(1) + IADD
               BUFFER(3) = BUFFER(3) - IADD / BUFFER(9)
               END IF
            END IF
C                                       Read or write the large
C                                       table record directly.
C                                       Write table record.
         IF (IOP.GE.2) THEN
            TRNO = MRNO
            INDEX = 1
            NWORD = BUFFER(72) - 1
            OP = 'WRIT'
            DO 10 I = 1,LIMIT
               IF (I.LT.LIMIT) THEN
                  CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), TRNO,
     *               RECORD(INDEX), IER)
C                                       insert flagging
               ELSE
                  CALL COPY (NWORD, RECORD(INDEX), SCRTCH)
                  IF (IOP.EQ.2) SCRTCH(1+NWORD) = 1
                  IF (IOP.EQ.3) SCRTCH(1+NWORD) = -1
                  CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), TRNO,
     *               SCRTCH, IER)
                  END IF
               IF (IER.NE.0) GO TO 990
               TRNO = TRNO + 1
               INDEX = INDEX + 256
               NWORD = NWORD - 256
 10            CONTINUE
            BUFFER(5) = MAX (BUFFER(5), LRNO)
C                                       Read table record.
         ELSE
            TRNO = MRNO
            INDEX = 1
            NWORD = BUFFER(72) - 1
            OP = 'READ'
            DO 20 I = 1,LIMIT
               IF (I.LT.LIMIT) THEN
                  CALL ZFIO ('READ', BUFFER(81), BUFFER(82), TRNO,
     *               RECORD(INDEX), IER)
C                                       insert flagging
               ELSE
                  CALL ZFIO ('READ', BUFFER(81), BUFFER(82), TRNO,
     *               SCRTCH, IER)
                  CALL COPY (NWORD, SCRTCH, RECORD(INDEX))
                  IF (SCRTCH(1+NWORD).LE.0) IERR = -1
                  END IF
               IF (IER.NE.0) GO TO 990
               TRNO = TRNO + 1
               INDEX = INDEX + 256
               NWORD = NWORD - 256
 20            CONTINUE
            END IF
         BUFFER(73) = MRNO
         BUFFER(75) = LLRNO
         GO TO 999
         END IF
C                                       New physical record
      IF (BUFFER(77).EQ.0) THEN
         TRNO = BUFFER(73)
      ELSE
         TRNO = BUFFER(78)
         END IF
      IF ((IRCODE.NE.BUFFER(77)) .OR. (MRNO.NE.TRNO)) THEN
C                                       write old one
         IF (BUFFER(71).EQ.2) THEN
            IF ((TRNO.GT.0) .AND. (BUFFER(77).GE.0) .AND.
     *         (BUFFER(77).LE.5)) THEN
               OP = 'WRIT'
               CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), TRNO,
     *            BUFFER(257), IER)
               IF (IER.NE.0) GO TO 990
               END IF
C                                       Expand if needed
            IF ((IRCODE.EQ.0) .AND. ((MRNO.GT.BUFFER(1)) .OR.
     *         (LRNO.GT.BUFFER(3)))) THEN
               IADD = BUFFER(42) / BUFFER(9) + 1
               CALL H2CHR (48, 1, BUFFER(17), PHNAME)
               CALL ZEXPND (BUFFER(81), BUFFER(32), PHNAME, IADD, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1050) IER
                  IERR = 5
                  GO TO 995
                  END IF
C                                       Update header info
               BUFFER(1) = BUFFER(1) + IADD
               BUFFER(3) = BUFFER(3) + IADD * BUFFER(9)
               END IF
            END IF
C                                       read new record
         OP = 'READ'
         TRNO = MRNO
         CALL ZFIO ('READ', BUFFER(81), BUFFER(82), TRNO, BUFFER(257),
     *      IER)
         IF (IER.NE.0) GO TO 990
         END IF
C                                       Update record pointers
 100  BUFFER(77) = IRCODE
      IF (IRCODE.EQ.0) THEN
         BUFFER(73) = MRNO
         BUFFER(75) = LLRNO
      ELSE
         BUFFER(78) = MRNO
         BUFFER(79) = LLRNO
         BUFFER(80) = IRCODE
         END IF
      IERR = 0
C                                       Move data RESULT <-> BUFFER
      IF (IRCODE.EQ.0) THEN
         IOFF = 256 + IOFF
         NWORD = BUFFER(72) - 1
         IF (IOP.GE.2) THEN
            CALL COPY (NWORD, RECORD, BUFFER(IOFF))
            IF (IOP.EQ.2) BUFFER(IOFF+NWORD) = 1
            IF (IOP.EQ.3) BUFFER(IOFF+NWORD) = -1
            TRNO = BUFFER(5)
            IF (LRNO.GT.TRNO) BUFFER(5) = LRNO
         ELSE
            CALL COPY (NWORD, BUFFER(IOFF), RECORD)
            IF (BUFFER(IOFF+NWORD).LE.0) IERR = -1
            END IF
C                                       Special records:
      ELSE IF (IRCODE.NE.2) THEN
         NWORD = 256
         IF (IRCODE.EQ.3) NWORD = 6
         IF (IRCODE.EQ.4) NWORD = 2
         IF (IRCODE.EQ.5) NWORD = 5
         IOFF = 256 + IOFF
         IF (IOP.EQ.1) THEN
            CALL COPY (NWORD, BUFFER(IOFF), RECORD)
         ELSE
            CALL COPY (NWORD, RECORD, BUFFER(IOFF))
            IF ((IRCODE.EQ.5) .AND. (LRNO.GT.BUFFER(53))) BUFFER(53) =
     *         LRNO
            END IF
         GO TO 999
C                                       Read data selection strings
      ELSE IF (IOP.EQ.1) THEN
         IOFF = 256 + IOFF
         NWORD = BUFFER(63+LRNO) - BUFFER(62+LRNO)
         IF (LRNO.EQ.8) NWORD = BUFFER(62) - BUFFER(70)
         I = MIN (NWORD, 513 - IOFF)
         CALL COPY (I, BUFFER(IOFF), RECORD)
         I = NWORD - I
         IF (I.GT.0) THEN
            TRNO = BUFFER(46) + 1
            OP = 'READ'
            CALL ZFIO ('READ', BUFFER(81), BUFFER(82), TRNO,
     *         BUFFER(257), IER)
            IF (IER.NE.0) GO TO 990
            BUFFER(78) = TRNO
            CALL COPY (I, BUFFER(257), RECORD)
            RECORD(NWORD+1) = 0
            END IF
C                                       write data selection strings
      ELSE
         NWORD = 0
         DO 135 I = 1,100
            IF (RECORD(I).EQ.0) GO TO 140
               CALL H2CHR (4, 1, RECORD(I), CTEST)
               IF (CTEST.NE.' ') NWORD = I
 135        CONTINUE
 140     I = NWORD + BUFFER(62+LRNO)
         IF (I.GT.513) I = 513
         BUFFER(62) = I
         IF (LRNO.LT.8) BUFFER(63+LRNO) = I
         IOFF = 256 + IOFF
         NWORD = BUFFER(62) - BUFFER(62+LRNO)
         IF (NWORD.LE.0) BUFFER(61) = BUFFER(61) - 1
         I = MIN (513-IOFF, NWORD)
         IF (I.GT.0) CALL COPY (I, RECORD, BUFFER(IOFF))
         IOFF = I + 1
         I = NWORD - I
         IF (I.GT.0) THEN
            TRNO = BUFFER(78)
            OP = 'WRIT'
            CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), TRNO,
     *         BUFFER(257), IER)
            IF (IER.NE.0) GO TO 990
            CALL RFILL (256, HBLANK, BUFFER(257))
            CALL COPY (I, RECORD(IOFF), BUFFER(257))
            BUFFER(78) = BUFFER(46) + 1
            END IF
         END IF
      GO TO 999
C                                       CLOSe: update on WRITes
 200  IF (BUFFER(71).GE.2) THEN
C                                       reset to no-op
         BUFFER(71) = 0
         OP = 'WRIT'
         CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), 1, BUFFER, IER)
         IF (IER.NE.0) GO TO 990
         IF (BUFFER(77).NE.0) THEN
            LIMIT = 1
            TRNO = BUFFER(78)
         ELSE
            LIMIT = -BUFFER(9)
            LIMIT = MAX (1, LIMIT)
            TRNO = BUFFER(73)
            END IF
         IF (TRNO.GT.0) THEN
            DO 210 I = 1,LIMIT
               INDEX = I*256 + 1
               CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), TRNO,
     *            BUFFER(INDEX), IER)
               IF (IER.NE.0) GO TO 990
               TRNO = TRNO + 1
 210           CONTINUE
            END IF
C                                       Does file need a compress?
 220     MRNO = BUFFER(3)
         LRNO = BUFFER(5)
         IF (LRNO.LE.0) GO TO 290
         IF (BUFFER(9).GT.0) THEN
            TRNO = (MRNO - LRNO) / BUFFER(9)
         ELSE
            TRNO = MRNO - LRNO
            END IF
C                                       Yes: do it
         IF (TRNO.GT.6) THEN
            IF (BUFFER(9).GT.0) THEN
               TRNO = (LRNO-1) / BUFFER(9) + 1
            ELSE
               TRNO = -LRNO * BUFFER(9)
               END IF
            ISIZE = TRNO + BUFFER(50) - 1
            CALL H2CHR (48, 1, BUFFER(17), PHNAME)
            CALL ZCMPRS (BUFFER(32), PHNAME, BUFFER(81), ISIZE, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1220) IER
               CALL MSGWRT (8)
               GO TO 290
               END IF
C                                       Update header, to disk
            XRNO = ISIZE
            BUFFER(1) = XRNO
            TRNO = XRNO - BUFFER(50) + 1
            IF (BUFFER(9).GT.0) THEN
               TRNO = TRNO * BUFFER(9)
            ELSE
               TRNO = -TRNO / BUFFER(9)
               END IF
            BUFFER(3) = TRNO
            TRNO = 1
            CALL ZFIO ('WRIT', BUFFER(81), BUFFER(82), 1, BUFFER, IER)
            IF (IER.NE.0) GO TO 990
            END IF
         END IF
C                                       actual close
 290  CALL ZCLOSE (BUFFER(81), BUFFER(82), IER)
      IERR = 0
      GO TO 999
C                                       Errors: bad codes
 970  WRITE (MSGTXT,1970) IRCODE, OPCODE
      GO TO 995
C                                       bad record number
 980  WRITE (MSGTXT,1980) LRNO, MRNO
      GO TO 995
C                                       I/O errors
 990  WRITE (MSGTXT,1990) IER, OP, TRNO
      IERR = 3
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('TABIO: ERROR',I5,' EXPANDING TABLE FILE')
 1220 FORMAT ('TABIO: ERROR',I5,' COMPRESSING TABLE FILE ON CLOSE')
 1970 FORMAT ('TABIO: BAD CODES - TYPE, OP =',I7,2X,A4)
 1980 FORMAT ('TABIO: BAD LRNO=',I12,' LIMIT',I12)
 1990 FORMAT ('TABIO: I/O ERROR',I5,' ON ',A4,' PHYS REC',I12)
      END
