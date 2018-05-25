      SUBROUTINE TAPIO (OP, FDVEC, BUFF, IBIND, IERR)
C-----------------------------------------------------------------------
C! read/writes tape and FITS disk files
C# tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2003-2004, 2009
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
C   TAPIO is primarily for reading and writing tapes but will also
C   work for disks for FITS files etc.  Disk files will be created and
C   expanded as necessary.  When a disk file is being written it is
C   compressed to its actual size when it is closed.
C   NOTE: TAPIO WORKS IN REAL (8-BIT) BYTES, NOT THE AIPS HALF-INTEGER
C   "BYTES".
C   Usage notes:
C     1) Zero fill FDVEC before filling in relevant values.
C     2) Opening the file.  If TAPIO determines that the file is not
C        open it will do so.  For disk files being opened for write
C        the file will be created.  Once the file is open the file
C        descriptor vector FDVEC must be used in each call.
C     3) Initialization.  TAPIO initializes the I/O using the values in
C        FDVEC when it opens the file.
C        If OP='OPxx' the file is created/opened but I/O is not
C        initialized; this allows positioning tapes before the actual
C        I/O starts. 'OPRD' means open for read, 'OPWT' means open for
C        write; 'OPWT' will cause the output file to be created if the
C        output is to disk.
C     4) Re-initialization.  If OP='BAKF' the file is repositioned at
C        the beginning of the current file, I/O will be reinitialized at
C        the next read call.  If OP='ADVF' then the tape is advanced to
C        the beginning of the next file and I/O is reinitialized at the
C        next read call.  These operations are only relevant to reading
C        files.
C     5) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.
C     6) Writing variable length records.  If variable length records
C        are to be read or written FDVEC(19) should be set to 1.
C        On WRITE, the actual record size is assumed to be given by
C        the first I   word of the record.  TAPIO will not handle
C        blocked, variable length records.
C     7) Limited number of logical records.  If only a fixed number of
C        logical records are to be processed before reinitialization
C        put the number of such records in FDVEC(20).  After this number
C        of transfers the I/O will be left in an uninitialized state and
C        on the next call will be reinitialized (e.g. can change logical
C        record length).  DO NOT USE THIS WITH BLOCKING HOWEVER, the
C        last block will not be the correct length.
C
C   Tapes:  129-NTAPED <= LUN <= 128
C
C   Disks: any other LUN
C          for FITS format only (no blocking)
C          TAPIO requires that FDVEC(2) = 2880 OR 2560,
C                              FDVEC(31) = 0 on input
C          TAPIO sets FDVEC(6) = 1
C          The desired file name must be in FDVEC(7-30) packed string
C
C   Inputs:
C      OP     C*4     Operation code: 'READ','WRIT','CLOS','BAKF',
C                                     'ADVF',
C                                     'OPRD','OPWT' open for rd,wrt
C                                     'FLSH' flush write buffer, Weof
C   Input/Output:
C      FDVEC  I(50)   File descriptor vector.
C                     1 = LUN to use, set before first call.
C                         129-NTAPED to 128 => tape, else disk.
C                     2 = Logical record length in bytes (8-bit)
C                   3-4 = Buffer size in 8-bit bytes I
C                     5 = volumn number (disk or tape)
C                     6 = blocking factor (0=>1). Value returned on read
C                  7-30 = File name for disk files (24 char. packed)
C                    31 = 0 if fixed length, 1 if variable length
C                    32 = Max. number of logical records to process
C                         0 => infinity
C                 33-39   Reserved for future use
C                The following are used by TAPIO:
C                    40 = FTAB pointer
C                    41 = Number of logical records left to process
C                         (negative => ignore)
C                    42 = LBYTES - number of bytes read or written.
C                 43-50 = reserved for future use
C      BUFF   R(*)   Buffer for I/O must be large enough for the largest
C                    transfer rounded up to the next larger number of
C                    disk blocks.
C   Outputs:
C      IBIND  I      The location in BUFF of the start of the next
C                    record. Before the first write call this should
C                    be set to 1 to determine where to start filling
C                    BUFF. Note: IBIND points to the address in the
C                    I   array irregardless of the actual data type.
C     IERR    I      Error return: 0 => ok
C                             1 => error creating file
C                             2 => input error
C                             3 => i/o error on initialize
C                             4 => end of file
C                             5 => beginning of medium
C                             6 => end of medium
C                             7 => Buffer too small
C                             8 => error opening file.
C                             9 => error expanding
C                            10 => data record shorter than 1 logical
C   Usage notes: For map i/o the first 16 words in each FTAB entry
C   contain a user table to handle double buffer i/o,  the rest
C   contain system-dependent I/O tables.
C   FTAB user table   entries, with offsets from the FIND pointer are:
C      FTAB + 0 =>  LUN using this entry
C             1 =>  Number of blocks to extend the file when full
C             2 =>  Number of 8-bit bytes in a logical record
C             3 =>  Number of disk logical records in each transfer (1)
C             4 =>  Which buffer half currently doing  i/o; -1 =>
C                   single buffer,  the other buffer half is available.
C           5-6 =>  Block offset on disk file for next operation I
C           7-8 =>
C             9 =>  I/O opcode 0=read, 1=write
C            10 =>  1 => tape, 2 => disk
C            11 =>  Number of logical records per physical
C            12 =>  Number of logical records done for this physical.
C            13 =>  1 => I/O active, else inactive (not initialized).
C            14 =>  number bytes last read/write to buffer 1
C            15 =>  number bytes last read/write to buffer 2
C   To suppress messages about logical and physical record lengths
C   being incosistent, set MSGSUP to 1000 or greater.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), BUFF(*), IBIND, IERR
      CHARACTER OP*4
C
      CHARACTER PHNAME*48
      LOGICAL   TAPE, DISK, DOREAD, DOWRIT
      INTEGER   JERR, IND, JBUF, MBYTE, I, NUMMSG, LBYTES, NRBPS,
     *   BUFISZ, NBITAB, NIPLR, IBY, I4TEMP, BUFSZ4, RECNO, BUFPNT,
     *   BUFPOL, NBYPWD, NFILES
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      SAVE NUMMSG
C-----------------------------------------------------------------------
      IERR = 2
      IBIND = 1
C                                       Initial parameters
      IND = FDVEC(40)
      NBITAB = NBITWD / 2
      NBYPWD = NBITWD / 8
      NRBPS = (NBPS * NBITAB) / 8
      NIPLR = (FDVEC(2) * 8 - 1) / NBITWD + 1
C                                       See if tape or disk.
C??                                     BETTER TEST HERE
      TAPE = (FDVEC(1).LT.129) .AND. (FDVEC(1).GE.129-NTAPED)
      DISK = .NOT.TAPE
      IF ((DISK) .AND. (FDVEC(2).NE.2880) .AND. (FDVEC(2).NE.2560))
     *   GO TO 985
      IF ((DISK) .AND. (FDVEC(31).NE.0)) GO TO 985
      IF (DISK) FDVEC(6) = 1
      BUFSZ4 = FDVEC(3)
      IF (BUFSZ4.GE.65536) BUFSZ4 = 65536 - 2 * NBYPWD
      FDVEC(3) = BUFSZ4
      BUFISZ = BUFSZ4 / NBYPWD
C??                                     CHECK HERE ON DEVTAB VALUE
      IF (MOD (DEVTAB(FDVEC(1)),2).EQ.1) GO TO 999
C                                       Test OPcode
      DOREAD = (OP.EQ.'OPRD') .OR. (OP.EQ.'READ')
      DOWRIT = (OP.EQ.'OPWT') .OR. (OP.EQ.'WRIT')
      IF ((DOWRIT) .OR. (DOREAD) .OR. (OP.EQ.'CLOS') .OR. (OP.EQ.'BAKF')
     *    .OR. (OP.EQ.'ADVF') .OR. (OP.EQ.'FLSH')) GO TO 10
         WRITE (MSGTXT,1000) OP
         GO TO 990
C                                       Check if CLOSE
 10   IERR = 0
      IF (OP.EQ.'CLOS') GO TO 900
      IF (OP.EQ.'FLSH') GO TO 900
C                                       Check if BAKF or ADVF
      IF ((OP.EQ.'BAKF') .OR. (OP.EQ.'ADVF')) GO TO 800
C                                       Check if Open but I/O inactive
      IF (((IND.GT.0) .AND. (FDVEC(1).EQ.FTAB(IND))) .AND.
     *   (FTAB(IND+13).NE.1)) GO TO 100
C                                       See if open and active.
      IF ((IND.GT.0) .AND. (FDVEC(1).EQ.FTAB(IND))) GO TO 500
C                                       Create/open/init
         PHNAME = ' '
         IF (TAPE) THEN
            CALL ZPHFIL ('MT', FDVEC(5), 1, 1, PHNAME, JERR)
            CALL CHR2H (48, PHNAME, 1, FDVEC(7))
            END IF
         CALL H2CHR (48, 1, FDVEC(7), PHNAME)
         IF (DOREAD) CALL ZTPOPN (FDVEC(1), IND, FDVEC(5), PHNAME,
     *      'READ', JERR)
         IF (DOWRIT) CALL ZTPOPN (FDVEC(1), IND, FDVEC(5), PHNAME,
     *      'WRIT', JERR)
C                                       Save number of blocks in file
         IF (JERR.NE.0) THEN
            IERR = 8
            WRITE (MSGTXT,1010) JERR
            FDVEC(40) = 0
            GO TO 990
            END IF
         FDVEC(40) = IND
C                                       Init previously inactive I/O
 100  CONTINUE
         FDVEC(42) = 0
         NUMMSG = -2
C                                       Fill values in FTAB
C                                       LUN
         FTAB(IND) = FDVEC(1)
C                                       Number of blocks to expand
         FTAB(IND+1) = 1000
C                                       Number of bytes in a logical
C                                       record.
         FTAB(IND+2) = FDVEC(2)
C                                       No. logical records per physical
         IF (FDVEC(6).LE.1) FDVEC(6) = 1
         FTAB(IND+11) = FDVEC(6)
C                                       Number of disk blocks per op.
         I4TEMP = FTAB(IND+11) * FTAB(IND+2)
         FTAB(IND+3) = 1
         FTAB(IND+13) = 0
C                                       Buffer size check
         FTAB(IND+4) = 1
         IF (BUFSZ4.GE.(2*I4TEMP)) GO TO 120
            FTAB(IND+4) = -1
C                                       Buffer too small
            IF ((BUFSZ4.GE.I4TEMP) .AND. (I4TEMP.LE.32766)) GO TO 120
               IERR = 7
               WRITE (MSGTXT,1100) BUFSZ4, I4TEMP
               IF (I4TEMP.GT.32766) WRITE (MSGTXT,1101) I4TEMP
               GO TO 990
C                                       Next disk block
 120     RECNO = 1
         FTAB(IND+5) = RECNO
C                                       Opcode
         FTAB(IND+9) = 0
         IF (DOWRIT) FTAB(IND+9) = 1
C                                       Medium type
         FTAB(IND+10) = 2
         IF (TAPE) FTAB(IND+10) = 1
C                                       No. logical rec. done
         FTAB(IND+12) = 0
C                                       Set buffer pointer
         BUFPNT = 1
C                                       Check limited no. records
         FDVEC(41) = -1
         IF (FDVEC(32).GT.0) FDVEC(41) = FDVEC(32)
C                                       Done if OP='OPxx'
         IF (OP(1:2).EQ.'OP') GO TO 999
C                                       I/O init w READ/WRIT call
C                                       Set I/O active flag
            FTAB(IND+13) = 1
C                                       WRITE
            IF (OP.NE.'WRIT') GO TO 150
C                                       If double buffering dummy
C                                       write to buffer 2
               IF (FTAB(IND+4).LE.0) GO TO 140
                  JBUF = 2
                  MBYTE = 0
                  FTAB(IND+4) = JBUF
                  FTAB(IND+13+JBUF) = MBYTE
                  CALL ZTPMIO ('WRIT', FTAB(IND), IND, MBYTE, BUFF,
     *               JBUF, IERR)
                  IF (IERR.NE.0) GO TO 980
C                                       Update pointers
 140           FTAB(IND+12) = FTAB(IND+12) + 1
               IF ((FTAB(IND+12)).GE.FTAB(IND+11)) BUFPNT = BUFISZ/2 + 1
               IF (FTAB(IND+4).LE.0) BUFPNT = 1
               IF ((FTAB(IND+12)).GE.FTAB(IND+11)) FTAB(IND+12) = 0
               IBIND = FTAB(IND+12) * NIPLR + BUFPNT
C                                       Check if done
               IF (FDVEC(41).GT.0) FDVEC(41)= FDVEC(41) - 1
               IF (FDVEC(41).EQ.0) FTAB(IND+13) = 0
C                                       Check if time to write
               IF ((FTAB(IND+11).GE.2) .AND. (FDVEC(41).NE.0)) GO TO 999
C                                       Wait for dummy write
                  IF (FTAB(IND+4).LE.0) GO TO 145
                     CALL ZTPWAT (FTAB(IND), IND, FTAB(IND+4), LBYTES,
     *                  IERR)
                     IF (IERR.EQ.4) GO TO 998
                     IF (IERR.NE.0) GO TO 980
                     FDVEC(42) = LBYTES
C                                       Write first buffer load.
 145              JBUF = 1
                  FTAB(IND+4) = JBUF
                  IF (FDVEC(31).EQ.1) THEN
                     CALL ZI16IL (1, 1, BUFF, IBY)
                     MBYTE = IBY
                  ELSE
                     MBYTE = FTAB(IND+2) * FTAB(IND+11)
                     END IF
                  FTAB(IND+13+JBUF) = MBYTE
                  CALL ZTPMIO ('WRIT', FTAB(IND), IND, MBYTE, BUFF,
     *               JBUF, IERR)
                  IF (IERR.EQ.4) GO TO 998
                  IF (IERR.NE.0) GO TO 980
C                                       Update record pointer
                  RECNO = RECNO + FTAB(IND+3)
                  FTAB(IND+5) = RECNO
                  GO TO 999
C                                       Start first READ
 150        CONTINUE
               JBUF = 1
               MBYTE = BUFSZ4
               IF (FTAB(IND+4).GT.0) MBYTE = MBYTE / 2
               IF (DISK) MBYTE = FDVEC(2)
               FTAB(IND+13+JBUF) = MBYTE
               CALL ZTPMIO ('READ', FTAB(IND), IND, MBYTE, BUFF, JBUF,
     *            IERR)
               IF (IERR.EQ.4) GO TO 998
               IF (IERR.NE.0) GO TO 980
C                                       Update block counter
               RECNO = RECNO + FTAB(IND+3)
               FTAB(IND+5) = RECNO
C                                       Set pointers etc.
               IBIND = 1
C                                       Wait for first
               JBUF = 1
               CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, IERR)
               IF (IERR.EQ.4) GO TO 998
               IF (IERR.EQ.0) GO TO 180
                  WRITE (MSGTXT,1600) IERR, OP
                  GO TO 990
C                                       Check bytes read
 180           FDVEC(42) = LBYTES
               IF ((LBYTES.LE.0) .AND. (MBYTE.GT.0)) GO TO 970
               IF (LBYTES.EQ.MBYTE) GO TO 190
                  IF (FDVEC(31).EQ.0) THEN
                     FTAB(IND+11) = LBYTES / FTAB(IND+2)
                     IF (FTAB(IND+11)*FTAB(IND+2).LT.LBYTES-2) THEN
                        NUMMSG = NUMMSG + 1
                        IF ((NUMMSG.LE.3) .AND. (MSGSUP.LT.1000)) THEN
                           WRITE (MSGTXT,1180) LBYTES, FTAB(IND+2)
                           CALL MSGWRT (6)
                           END IF
                        IERR = 10
                        END IF
                     IF (FTAB(IND+11).EQ.0) FTAB(IND+11) = 1
                  ELSE IF (NUMMSG.LE.1) THEN
                     CALL ZI16IL (1, 1, BUFF, IBY)
                     I = IBY
                     IF ((I.GT.FTAB(IND+2)) .OR. (ABS(I-LBYTES).GT.2))
     *                  THEN
                        NUMMSG = NUMMSG + 1
                        IF (LBYTES.GT.FTAB(IND+2)) THEN
                           WRITE (MSGTXT,1185) LBYTES, I, FTAB(IND+2)
                        ELSE
                           WRITE (MSGTXT,1186) LBYTES, I
                           END IF
                        CALL MSGWRT (6)
                        END IF
                     END IF
C                                       Check if done
 190           IF (FDVEC(41).GT.0) FDVEC(41)= FDVEC(41) - 1
               IF (FDVEC(41).EQ.0) THEN
                  FTAB(IND+13) = 0
                  GO TO 999
                  END IF
C                                       Done if single buffering
C                                       If double buffer and reading
C                                       start 2nd read
               IF (FTAB(IND+4).GE.0) THEN
                  JBUF = 2
                  FTAB(IND+4) = JBUF
                  BUFPNT = BUFISZ / 2 + 1
                  MBYTE = BUFSZ4 / 2
                  IF (DISK) MBYTE = FDVEC(2)
                  FTAB(IND+13+JBUF) = MBYTE
                  CALL ZTPMIO ('READ', FTAB(IND), IND, MBYTE,
     *               BUFF(BUFPNT), JBUF, JERR)
                  IF (JERR.NE.0) IERR = JERR
                  IF (JERR.EQ.4) GO TO 998
                  IF (JERR.NE.0) GO TO 980
C                                       Update block counter
                  RECNO = RECNO + FTAB(IND+3)
                  FTAB(IND+5) = RECNO
                  END IF
               GO TO 999
C-----------------------------------------------------------------------
C                                       Do later I/O operations
C                                       Set buffer pointer.
 500  CONTINUE
         BUFPOL = 1
         IF (FTAB(IND+4).EQ.1) BUFPOL = BUFISZ / 2 + 1
         FTAB(IND+12) = FTAB(IND+12) + 1
C                                       More logical records in physical
C                                       record.
         IF (FTAB(IND+12).GE.FTAB(IND+11)) GO TO 520
            IBIND = FTAB(IND+12) * NIPLR + BUFPOL
C                                       Check if done
            IF (FDVEC(41).GT.0) FDVEC(41)= FDVEC(41) - 1
            IF (FDVEC(41).EQ.0) FTAB(IND+13) = 0
            IF ((FDVEC(41).EQ.0) .AND. (OP.EQ.'WRIT')) GO TO 900
            GO TO 999
C                                       I/O time.
 520     RECNO = FTAB(IND+5)
C                                       Wait on last I/O if double
C                                       buffer.
 600     BUFPNT = 1
         IF (FTAB(IND+4).LT.0) GO TO 650
            JBUF = FTAB(IND+4)
            IF (JBUF.EQ.2) BUFPNT = BUFISZ / 2 + 1
            MBYTE = FTAB(IND+13+JBUF)
            CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, IERR)
            IF (IERR.EQ.4) GO TO 998
            IF (IERR.EQ.0) GO TO 640
               WRITE (MSGTXT,1600) IERR, OP
               GO TO 990
C                                       Check bytes
 640        FDVEC(42) = LBYTES
            IF ((LBYTES.LE.0) .AND. (MBYTE.GT.0)) GO TO 970
            IF (LBYTES.NE.MBYTE) THEN
               IF (FTAB(IND+9).NE.0) THEN
                  WRITE (MSGTXT,1640) LBYTES, MBYTE
                  IERR = 6
                  GO TO 990
               ELSE
                  IF (FDVEC(31).EQ.0) THEN
                     FTAB(IND+11) = LBYTES / FTAB(IND+2)
                     IF (FTAB(IND+11)*FTAB(IND+2).LT.LBYTES-2) THEN
                        NUMMSG = NUMMSG + 1
                        IF ((NUMMSG.LE.3) .AND. (MSGSUP.LT.1000)) THEN
                           WRITE (MSGTXT,1180) LBYTES, FTAB(IND+2)
                           CALL MSGWRT (6)
                           END IF
                        IERR = 10
                        END IF
                     IF (FTAB(IND+11).EQ.0) FTAB(IND+11) = 1
                  ELSE IF (NUMMSG.LE.1) THEN
                     CALL ZI16IL (1, 1, BUFF(BUFPNT), IBY)
                     I = IBY
                     IF ((I.GT.FTAB(IND+2)) .OR. (ABS(I-LBYTES).GT.2))
     *                  THEN
                        NUMMSG = NUMMSG + 1
                        IF (LBYTES.GT.FTAB(IND+2)) THEN
                           WRITE (MSGTXT,1185) LBYTES, I, FTAB(IND+2)
                        ELSE
                           WRITE (MSGTXT,1186) LBYTES, I
                           END IF
                        CALL MSGWRT (6)
                        END IF
                     END IF
                  END IF
               END IF
C                                       Check if done
 650     IBIND = BUFPNT
         IF (FDVEC(41).GT.0) FDVEC(41)= FDVEC(41) - 1
         IF (FDVEC(41).EQ.0) FTAB(IND+13) = 0
         I = 10000
         IF ((FDVEC(41).GE.0) .AND. (OP.EQ.'READ')) I = FDVEC(41) + 1
         IF ((FDVEC(41).GE.0) .AND. (OP.EQ.'READ') .AND.
     *      (FTAB(IND+4).GT.0)) I = FDVEC(41) + 1 - FTAB(IND+11)
         IF (I.LE.0) FTAB(IND+12) = 0
C                                       Do I/O
         IF (I.LE.0) GO TO 999
            MBYTE = BUFSZ4
            IF (FTAB(IND+4).GT.0) MBYTE = MBYTE / 2
            IF (OP.EQ.'WRIT') MBYTE = FTAB(IND+2) * FTAB(IND+12)
            IF ((FDVEC(31).EQ.1) .AND. (OP.EQ.'WRIT')) THEN
               CALL ZI16IL (1, 1, BUFF(BUFPOL), MBYTE)
               END IF
            IF (DISK) MBYTE = FDVEC(2)
            FTAB(IND+12) = 0
            IF (FTAB(IND+4).GT.0) FTAB(IND+4) = 3 - FTAB(IND+4)
            JBUF = 1
            IF (FTAB(IND+4).GT.0) JBUF = FTAB(IND+4)
            FTAB(IND+13+JBUF) = MBYTE
            CALL ZTPMIO (OP, FTAB(IND), IND, MBYTE, BUFF(BUFPOL), JBUF,
     *         JERR)
            IF (JERR.NE.0) IERR = JERR
            IF (JERR.EQ.4) GO TO 998
            IF (JERR.NE.0) GO TO 980
C                                       Update block counter
            RECNO = RECNO + FTAB(IND+3)
            FTAB(IND+5) = RECNO
C                                       If this is last, quit
            IF ((FDVEC(41).EQ.0) .AND. (OP.EQ.'WRIT')) GO TO 900
C                                       If single buffering, wait
            IF (FTAB(IND+4).GT.0) GO TO 999
               CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, JERR)
               IF (JERR.NE.0) IERR = JERR
               IF (JERR.EQ.4) GO TO 998
               IF (JERR.NE.0) GO TO 980
               FDVEC(42) = LBYTES
C                                       Check bytes read
               IF ((LBYTES.LE.0) .AND. (MBYTE.GT.0)) GO TO 970
               IF (LBYTES.NE.MBYTE) THEN
                  IF (FTAB(IND+9).NE.0) THEN
                     WRITE (MSGTXT,1640) LBYTES, MBYTE
                     IERR = 6
                     GO TO 990
                  ELSE
                     IF (FDVEC(31).EQ.0) THEN
                        FTAB(IND+11) = LBYTES / FTAB(IND+2)
                        IF (FTAB(IND+11)*FTAB(IND+2).LT.LBYTES-2) THEN
                           NUMMSG = NUMMSG + 1
                           IF ((NUMMSG.LE.3) .AND. (MSGSUP.LT.1000))
     *                        THEN
                              WRITE (MSGTXT,1180) LBYTES, FTAB(IND+2)
                              CALL MSGWRT (6)
                              END IF
                           IERR = 10
                           END IF
                        IF (FTAB(IND+11).EQ.0) FTAB(IND+11) = 1
                     ELSE IF (NUMMSG.LE.1) THEN
                        CALL ZI16IL (1, 1, BUFF, IBY)
                        I = IBY
                        IF ((I.GT.FTAB(IND+2)) .OR.
     *                     (ABS(I-LBYTES).GT.2)) THEN
                           NUMMSG = NUMMSG + 1
                           IF (LBYTES.GT.FTAB(IND+2)) THEN
                              WRITE (MSGTXT,1185) LBYTES, I, FTAB(IND+2)
                           ELSE
                              WRITE (MSGTXT,1186) LBYTES, I
                              END IF
                           CALL MSGWRT (6)
                           END IF
                        END IF
                     END IF
                  END IF
               GO TO 999
C-----------------------------------------------------------------------
C                                       BAKF operation
C                                       Reset record pointer
 800  RECNO = 1
      FDVEC(42) = 0
      IERR = 8
      IF ((IND.LE.0) .OR. (FTAB(IND).NE.FDVEC(1))) GO TO 999
      IERR = 0
      FTAB(IND+5) = RECNO
      IBIND = 1
      NFILES = 1
C                                       Wait on any pending read.
      IF ((FTAB(IND+4).LE.0) .OR. (FTAB(IND+13).NE.1)) GO TO 810
         CALL ZTPWAT (FTAB(IND), IND, FTAB(IND+4), LBYTES, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.4)) GO TO 980
         IF (IERR.EQ.4) THEN
            IF (OP.EQ.'ADVF') NFILES = 0
            IF (OP.EQ.'BAKF') NFILES = 2
            END IF
C                                       Reset I/O active flag
 810  FTAB(IND+13) = 0
      FTAB(IND+12) = 0
C                                       Reposition tape.
      IF ((FTAB(IND+10).EQ.1) .AND. (NFILES.GT.0)) THEN
         CALL ZTAPE (OP, FTAB(IND), IND, NFILES, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1810) IERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close, flush
C                                       Wait for pending I/O
 900  IERR = 8
      FDVEC(42) = 0
      IF ((IND.LE.0) .OR. (FTAB(IND).NE.FDVEC(1))) GO TO 999
      IERR = 0
      IF (FTAB(IND+4).LE.0) GO TO 910
         CALL ZTPWAT (FTAB(IND), IND, FTAB(IND+4), LBYTES, IERR)
         IF ((IERR.EQ.4) .AND. (FTAB(IND+9).EQ.0)) IERR = 0
         IF (IERR.NE.0) GO TO 930
         FDVEC(42) = LBYTES
C                                       For blocked writes, flush
C                                       buffer.
 910  IF ((FTAB(IND+9).EQ.0) .OR. (FTAB(IND+11).LE.1)) GO TO 930
C                                       Check that something is in the
C                                       buffer.
      IF (FTAB(IND+12).LE.0) GO TO 930
         MBYTE = FTAB(IND+2) * FTAB(IND+12)
         IF (FTAB(IND+4).GT.0) FTAB(IND+4) = 3 - FTAB(IND+4)
         JBUF = 1
         IF (FTAB(IND+4).GT.0) JBUF = FTAB(IND+4)
         BUFPNT = 1 + (JBUF - 1) * (BUFISZ / 2)
         CALL ZTPMIO ('WRIT', FTAB(IND), IND, MBYTE, BUFF(BUFPNT), JBUF,
     *      JERR)
         IF (IERR.EQ.0) IERR = JERR
         IF (JERR.NE.0) GO TO 930
         CALL ZTPWAT (FTAB(IND), IND, JBUF, LBYTES, JERR)
         IF (IERR.EQ.0) IERR = JERR
C                                       Turn off things in case CLOS
C                                       follows FLSH
 930  FTAB(IND+4) = 0
      FTAB(IND+12) = 0
      IF (IERR.NE.0) GO TO 980
      IF (OP.EQ.'WRIT') GO TO 999
C                                       WEOF
      IF ((FTAB(IND+11).LE.0) .OR. (DISK) .OR. (FTAB(IND+9).EQ.0))
     *   GO TO 935
         I = 1
         CALL ZTAPE ('WEOF', FTAB(IND), IND, I, IERR)
         IF (IERR.EQ.0) GO TO 935
            WRITE (MSGTXT,1930) IERR
            CALL MSGWRT (6)
 935  FTAB(IND+11) = 0
C                                       Close file
 950  IF ((OP.EQ.'FLSH') .AND. (IERR.NE.0)) GO TO 980
      IF (OP.EQ.'FLSH') GO TO 998
         FDVEC(40) = 0
         CALL ZTPCLS (FTAB(IND), IND, JERR)
         IF (IERR.EQ.0) IERR = JERR
         IF (IERR.NE.0) GO TO 980
         GO TO 998
C                                       I/O error
 970  WRITE (MSGTXT,1970) LBYTES
      IERR = 4
      GO TO 990
 980  WRITE (MSGTXT,1980) IERR, OP
      GO TO 990
C                                       disk IO error
 985  WRITE (MSGTXT,1985) FDVEC(2), FDVEC(31)
C                                       error messages
 990  CALL MSGWRT (6)
C                                       Mark I/O inactive
 998  FTAB(IND+13) = 0
C                                       Don't wait after error
      FTAB(IND+4) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPIO: OPCODE ',A4,' INVALID')
 1010 FORMAT ('TAPIO: ERROR ',I3,' OPENING FILE')
 1100 FORMAT ('TAPIO: BUFFER SIZE=',I6,' TOO SMALL, NEED',I6)
 1101 FORMAT ('TAPIO: RECORD SIZE=',I8,' TOO LARGE FOR I/O SYSTEM')
 1180 FORMAT ('TAPIO: RECORD LENGTH',I7,' INCONSISTENT WITH BLOCK SIZE',
     *   I6)
 1185 FORMAT ('TAPIO: LENGTH',I7,' INCONSISTENT W EXPECTED AND MAX',2I6)
 1186 FORMAT ('TAPIO: LENGTH',I7,' INCONSISTENT WITH EXPECTED',I6)
 1600 FORMAT ('TAPIO: ERROR ',I3,2X,A4,'ING FILE')
 1640 FORMAT ('TAPIO: WROTE',I6,' BYTES WHEN REQUESTED',I6,
     *   'END-OF-TAPE?')
 1810 FORMAT ('TAPIO: ERROR',I3,' POSITIONING TAPE')
 1930 FORMAT ('TAPIO: ERROR',I3,' WRITING END OF FILE MARKS')
 1970 FORMAT ('TAPIO: LBYTES=',I8,' BUT IERR = 0')
 1980 FORMAT ('TAPIO: ERROR',I3,' DOING I/O WITH OPCODE = ',A4)
 1985 FORMAT ('TAPIO: BYTES, TYPE=',I7,I3,
     *     ' SHOULD BE  2880 or 2560, 0')
      END
