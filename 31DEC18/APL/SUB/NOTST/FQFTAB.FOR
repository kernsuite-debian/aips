      SUBROUTINE FQFTAB (DISK, CNO, LUN, CATR, MAXFQI, NUMBIF, NUMBFQ,
     *   FQOFF, FQCHSP, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Return FQ id/IF table of frequency offsets and channel increments
C# Header Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C   Returns the number of IF and FQ ids from the Fq or CH table and
C   tables of the frequency offsets and the channel separations.
C   Input:
C      DISK     I        Disk number. If .le. 0 /CFILES/ scratch file
C                        number CNO will be used.
C      CNO      I        Catalog slot number
C      LUN      I        AIPS LUN to use for CHNDAT.
C      CATR     R(*)     Catalog header
C      MAXFQI   I        Maximum number of FQ ids.
C   Output:
C      NUMBIF   I        The number of IFs in the data set.
C      NUMBFQ   I        The maximum FQ if number in the data set or
C                        MAXFQI if this is smaller.
C      FQOFF    R(MAXIF,MAXFQI)  Frequency offset table from the
C                        reference frequency (Hz) by IF and FQ id.
C      FQCHSP   R(MAXIF,MAXFQI)  Channel separation table (Hz) by IF and
C                        FQ id.
C      BUFFER   I(512)   I/O work buffer.
C      IRET     I        Return error code, 0=>OK,
C                           1 => exceeded table size
C                           other = table I/O error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISK, CNO, LUN, MAXFQI, NUMBIF, NUMBFQ, BUFFER(*), IRET
      REAL      CATR(256), FQOFF(MAXIF,MAXFQI), FQCHSP(MAXIF,MAXFQI)
C
      INTEGER   IVER, IERR, IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC),
     *   NUMROW, LOOP, FQID, IFSIDE(MAXIF), LOOPIF
      LOGICAL   TABLE, EXIST, FITASC, ISFQ, ISCH
      REAL      IFCHW(MAXIF), IFTBW(MAXIF)
      DOUBLE PRECISION IFFREQ(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize output
      NUMBIF = 1
      NUMBFQ = 1
      DO 20 LOOPIF = 1,MAXIF
         DO 10 LOOP = 1,MAXFQI
            FQOFF(LOOPIF,LOOP) = 0.0
            FQCHSP(LOOPIF,LOOP) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       There are three different
C                                       possibilities here, either an FQ
C                                       table, a CH table or neither.
      IVER = 1
      CALL ISTAB ('FQ', DISK, CNO, IVER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IERR)
      ISFQ = EXIST .AND. TABLE .AND. (IERR.EQ.0)
      ISCH = .FALSE.
      IF (.NOT.ISFQ) THEN
         CALL ISTAB ('CH', DISK, CNO, IVER, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IERR)
         ISCH = EXIST .AND. TABLE .AND. (IERR.EQ.0)
         END IF
C                                       FQ Table branch
      IF (ISFQ) THEN
C                                       Init FQ table
         CALL FQINI ('READ', BUFFER, DISK, CNO, IVER, CATR, LUN,
     *      IFQRNO, FQKOLS, FQNUMV, NUMBIF, IRET)
         IF (IRET.NE.0) GO TO 999
         NUMROW = BUFFER(5)
C                                       Read table
         DO 100 LOOP = 1,NUMROW
            IFQRNO = LOOP
            CALL TABFQ ('READ', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMBIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.GT.0) GO TO 999
C                                       In case entries are flagged.
            IF (IRET.EQ.0) THEN
               NUMBFQ = MAX (NUMBFQ, FQID)
               IF (NUMBFQ.LE.MAXFQI) THEN
C                                       Save values
                  DO 50 LOOPIF = 1,NUMBIF
                     FQOFF(LOOPIF,FQID) = IFFREQ(LOOPIF)
                     FQCHSP(LOOPIF,FQID) = IFCHW(LOOPIF)
 50                  CONTINUE
               ELSE
C                                       Too many FQ ids.
                  NUMBFQ = MAXFQI
                  WRITE (MSGTXT,1050) FQID, MAXFQI
                  CALL MSGWRT (7)
                  END IF
               END IF
 100        CONTINUE
C                                       Close table
         CALL TABFQ ('CLOS', BUFFER, IFQRNO, FQKOLS, FQNUMV, NUMBIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       CH Table branch
      ELSE IF (ISCH) THEN
C                                       Only 1 FQ id in CH tables
         FQID = 1
         CALL CHNDAT ('READ', BUFFER, DISK, CNO, IVER, CATR, LUN,
     *      NUMBIF, IFFREQ, IFSIDE, FQCHSP, BNDCOD, FQID, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
C                                       Copy double precision to
C                                       output single precision
         ELSE
            DO 125 LOOPIF = 1, NUMBIF
               FQOFF(LOOPIF,FQID) = IFFREQ(LOOPIF)
 125           CONTINUE
            END IF
         END IF
C
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'THE ERROR OCCURED IN FQFTAB'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('FQFTAB: THERE ARE MORE FQ IDS (',I3,') THAN MAX.=',I3)
 1100 FORMAT ('FQFTAB: CHNDAT ERROR ',I3)
      END
