      SUBROUTINE AMERGE (KEY1, KEY2, NREC, LREC, NSORT, NBUFF, LUN, VOL,
     *   FILCNO, LENBU, INBUF, OUTBUF, JERR)
C-----------------------------------------------------------------------
C! sorts by merging previously sorted blocks of records
C# Sort UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000
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
C   Merge orders records of REAL values by merging previously sorted
C   blocks of records.  Sorts on values offset by KEY1 and KEY2 from
C   the first word in each record.  KEY2 advances fastest.  Ordering is
C   into decreasing values of the keys.  Data is expected and is
C   returned in the first file defined by VOL and FILCNO.  Two cataloged
C   SC files are required.  Buffers should be 2*sector length greater
C   than the minimum size sufficient for single buffered operation.
C   Input:
C      KEY1,KEY2   I    Offsets in I   words from first word of sort key
C      NREC        I    Number of records to be merged.
C      LREC        I    Number of I   words in a record.
C      NSORT       I    Number of records in a block previously sorted
C                       (Should be a multiple of LENBU))
C      NBUFF       I    Number of input streams to be used in the merge
C                       (adj. array dim.; max. 20).
C      LUN(NBUFF,2)I    LUNs for work files, each is opened NBUFF times.
C      LENBU       I    The number of records per buffer.
C   In/out (order may be switched on output):
C      VOL(2)      I    Volumne numbers of the work disks.
C      FILCNO(2)   I    Catalog numbers of the work files. (type 'SC')
C   Output:
C      INBUF(LREC,LENBU,NBUFF)  R    Work buffer.
C      OUTBUF(LREC,LENBU)       R    Work buffer.
C      JERR        I    Return error code: 0 = OK.
C                       2 => open error
C                       3 => I/O error
C   Programmer: W. D. Cotton, Jan. 1981.
C-----------------------------------------------------------------------
      INTEGER   NBUFF, KEY1, KEY2, NREC, LREC, NSORT, LUN(NBUFF,2),
     *   VOL(2), FILCNO(2), LENBU, JERR
      REAL      INBUF(*), OUTBUF(*)
C
      LOGICAL   T, F, EOI, EMPTY(20)
      INTEGER   OBO, IBO, ISECSZ, IROFF, OROFF, IBIND(20), OBIND, IN,
     *   OUT, BUFSZ, OCNT, OPOINT, IFIRST, FIND(20,2), INIO(20),
     *   IPOINT(20), JIN(20), I, IERR, IFILE, IND1, IND2, IPASS, KIN,
     *   NEXT, NIOUT, NPASS, X2READ, XLEFT
      DOUBLE PRECISION XNREC, SECSZ
      CHARACTER FILE(2)*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA OROFF, IBO /0, 1/
C-----------------------------------------------------------------------
      CALL ZPHFIL ('SC', VOL(1), FILCNO(1), 1, FILE(1), IERR)
      CALL ZPHFIL ('SC', VOL(2), FILCNO(2), 1, FILE(2), IERR)
C                                       Get D   record count.
      XNREC = NREC
C                                       Determine number of merge passes
      NPASS = LOG (XNREC/NSORT) / LOG(1.0D0*NBUFF) + 1.0D0
      NPASS = MAX (NPASS, 1)
C                                       Open each file NBUFF times.
      JERR = 0
      DO 20 I = 1,NBUFF
         IFILE = 1
         CALL ZOPEN (LUN(I,1), FIND(I,1), VOL(1), FILE(1), T, F, T,
     *      IERR)
         IF (IERR.LE.0) GO TO 10
            WRITE (MSGTXT,1000) IERR, IFILE, I
            CALL MSGWRT (8)
            JERR = 2
            GO TO 999
 10      IFILE = 2
         CALL ZOPEN (LUN(I,2), FIND(I,2), VOL(2), FILE(2), T, F, T,
     *      IERR)
         IF (IERR.LE.0) GO TO 20
            WRITE (MSGTXT,1000) IERR, IFILE, I
            CALL MSGWRT (8)
            JERR = 2
            GO TO 999
 20      CONTINUE
      IN = 1
      OUT = 2
      BUFSZ = 2 * (LREC * LENBU + NBPS)
C                                       Begin pass loop
      DO 300 IPASS = 1,NPASS
C                                       Determine input section size.
         SECSZ = NSORT
         SECSZ = SECSZ * (REAL(NBUFF)**(IPASS-1))
C                                       If section size .GE. data base
C                                       exit
         IF (SECSZ.GE.NREC) GO TO 900
         NIOUT = LENBU
         OBO = 1
C                                       Initialize output
         CALL UVINIT ('WRIT', LUN(1,OUT), FIND(1,OUT), NREC, OROFF,
     *      LREC, NIOUT, BUFSZ, OUTBUF, OBO, OBIND, IERR)
         IF (IERR.EQ.0) GO TO 110
            WRITE (MSGTXT,1020) IERR,IPASS
            CALL MSGWRT (8)
            JERR = 3
            GO TO 999
 110     OPOINT = OBIND
         OCNT = 0
C                                       Set initial rec. offset
         IROFF = 0
         EOI = F
C                                       Begin section loop.
 120     CONTINUE
C                                       Initialize inputs
            DO 140 I = 1,NBUFF
C                                       Set # rec per call
               INIO(I) = LENBU
C                                       Set stream empty marker.
               EMPTY(I) = EOI
C                                       Check if this stream necessary.
               IF (EOI) GO TO 140
C                                       See how many rec. left.
               XLEFT = NREC - IROFF
               X2READ = XLEFT
               IF (XLEFT.GT.SECSZ) X2READ = SECSZ
               ISECSZ = X2READ
C                                       See if end-of-data this sec.
               IF (XLEFT.LE.SECSZ) EOI = T
               KIN = (I-1) * (BUFSZ/2) + 1
               JIN(I) = KIN
               CALL UVINIT ('READ', LUN(I,IN), FIND(I,IN), ISECSZ,
     *            IROFF, LREC, INIO(I), BUFSZ, INBUF(KIN), IBO,
     *            IBIND(I), IERR)
               IF (IERR.EQ.0) GO TO 130
                  WRITE (MSGTXT,1120) IERR, I, IPASS
                  CALL MSGWRT (8)
                  JERR = 3
                  GO TO 999
C                                       Update record offset.
 130           IROFF = ISECSZ + IROFF
C                                       First read
               CALL UVDISK ('READ', LUN(I,IN), FIND(I,IN), INBUF(KIN),
     *            INIO(I), IBIND(I), IERR)
               IPOINT(I) = IBIND(I) + KIN - 1
               IF (IERR.EQ.0) GO TO 140
                  WRITE (MSGTXT,1130) IERR, I, IPASS
                  CALL MSGWRT (8)
                  JERR = 3
                  GO TO 999
 140           CONTINUE
C                                       Begin loop merging sections.
 150        CONTINUE
C                                       Check that there is data left an
C                                       find first stream with data left
            DO 160 I = 1,NBUFF
               IF (EMPTY(I)) GO TO 160
                  IFIRST = I + 1
                  NEXT = I
                  GO TO 200
 160           CONTINUE
C                                       If you got here then you're out
C                                       data.  Unless an EOI was encoun
C                                       go back and initialize the
C                                       of sections.
            IF (.NOT.EOI) GO TO 120
C                                       Finished this pass.
         GO TO 290
C                                       Find next largest record.
 200     IF (IFIRST.GT.NBUFF) GO TO 230
            DO 220 I = IFIRST,NBUFF
               IF (EMPTY(I)) GO TO 220
                  IND1 = IPOINT(I) - 1
                  IND2 = IPOINT(NEXT) - 1
                  IF (INBUF(IND1+KEY1) - INBUF(IND2+KEY1)) 220,205,210
 205              IF (INBUF(IND1+KEY2).LE.INBUF(IND2+KEY2)) GO TO 220
 210                 NEXT = I
 220           CONTINUE
C                                       Copy to output buffer
 230     CALL RCOPY (LREC, INBUF(IPOINT(NEXT)), OUTBUF(OPOINT))
C                                       Update pointers and counters.
         IPOINT(NEXT) = IPOINT(NEXT) + LREC
         OPOINT = OPOINT + LREC
         INIO(NEXT) = INIO(NEXT) - 1
         NIOUT = NIOUT - 1
         OCNT = OCNT + 1
C                                       Check if time to write output
         IF (NIOUT.GT.0) GO TO 240
            NIOUT = OCNT
            OCNT = 0
            CALL UVDISK ('WRIT', LUN(1,OUT), FIND(1,OUT), OUTBUF, NIOUT,
     *         OBIND, IERR)
            OPOINT = OBIND
            IF (IERR.EQ.0) GO TO 240
               WRITE (MSGTXT,1230) IERR, IPASS
               CALL MSGWRT (8)
               JERR = 3
               GO TO 999
C                                       Update pointer.
C                                       Check if time to read input.
 240     IF (INIO(NEXT).GT.0) GO TO 150
            KIN = JIN(NEXT)
            CALL UVDISK ('READ', LUN(NEXT,IN), FIND(NEXT,IN),
     *         INBUF(KIN), INIO(NEXT), IBIND(NEXT), IERR)
            IF (IERR.EQ.0) GO TO 250
               WRITE (MSGTXT,1130) IERR, NEXT, IPASS
               CALL MSGWRT (8)
               JERR = 3
               GO TO 999
C                                       Update pointers and counter.
 250        IPOINT(NEXT) = IBIND(NEXT) + KIN - 1
C                                       Check for end of input.
            IF (INIO(NEXT).LE.0) EMPTY(NEXT) = T
C                                       Loop back for next record.
            GO TO 150
C                                       Finish write.
 290     NIOUT = -OCNT
         CALL UVDISK ('FLSH', LUN(1,OUT), FIND(1,OUT), OUTBUF, NIOUT,
     *      OBIND, IERR)
         IF (IERR.EQ.0) GO TO 295
            WRITE (MSGTXT,1230) IERR, IPASS
            CALL MSGWRT (8)
            JERR = 3
            GO TO 999
C                                       Switch input and output.
 295     IN = 3 - IN
         OUT = 3 - OUT
 300     CONTINUE
C                                       Close files.
 900  DO 910 I = 1,NBUFF
         CALL ZCLOSE (LUN(I,1), FIND(I,1), IERR)
         CALL ZCLOSE (LUN(I,2), FIND(I,2), IERR)
 910     CONTINUE
C                                       If output currently on file 2
C                                       switch names etc.
      IF (IN.EQ.2) CALL FSWTCH (FILE(1), FILE(2), VOL(1), VOL(2),
     *   FILCNO(1), FILCNO(2), IBO, OBO)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AMERGE: ERROR',I7,' OPENING FILE ',I3,' NUMBER ',I3)
 1020 FORMAT ('AMERGE: ERROR',I7,' INIT OUTPUT PASS',I4)
 1120 FORMAT ('AMERGE: ERROR',I7,' INIT INPUT FILE',I3,' PASS ',I2)
 1130 FORMAT ('AMERGE: READ ERROR',I7,' FILE ',I3,' PASS ',I3)
 1230 FORMAT ('AMERGE: WRITE ERROR',I7,' PASS ',I4)
      END
