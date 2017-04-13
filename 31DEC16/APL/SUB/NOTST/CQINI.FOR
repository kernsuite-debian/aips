      SUBROUTINE CQINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICQRNO, CQKOLS, CQNUMV, NOIFCQ, IERR)
C----------------------------------------------------------------------
C! Creates and initializes a correlator parameter freq. (CQ) table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2006-2007
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
C   Creates and initializes a correlator parameter frequency (CQ) table
C   Inputs:
C      OPCODE    C*4     Operation code:
C                        'WRIT' = Create/init. for write and read
C                        'READ' = open for read only
C      BUFFER    I(512)  I/O buffer and related storage, also defines
C                        file if open.
C      DISK      I       Disk to use
C      CNO       I       Catalog slot number
C      CATBLK    I(256)  Catalog header block
C      LUN       I       Logical unit number to use
C   Input/output:
C      VER       I       CQ table version number.
C   Input (create) / output (pre-existing):
C      NOIFCQ    I       No. of IF's in table.
C   Output:
C      ICQRNO    I       next row number; start of file if READ, the
C                        (last+1) if WRIT
C      CQKOLS    I(*)    The column pointer array in order:
C                        FQ_ID, SUBARRAY, FFT_SIZE, NO_CHAN, SPEC_AVG,
C                        EDGE_FREQ, CHAN_BW, TAPER_FN, OVER_SAMPL,
C                        ZERO_PAD, TIME_FILT, TIME_AVG, NO_BITS,
C                        FFT_OVLAP
C      CQNUMV    I(*)    Element count in each column
C      IERR      I       Return error code, 0=> ok else error.
C---------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCQV.INC'
      CHARACTER OPCODE*4
      INTEGER BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICQRNO,
     *   CQKOLS(MAXCQC), CQNUMV(MAXCQC), NOIFCQ, IERR
C
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL DOREAD, NEWFIL
      CHARACTER LTCQ(MAXCQC)*24, LTTCQ*56, LUNTCQ(MAXCQC)*8,
     *   LKEYCQ(NKEYCQ)*8
      HOLLERITH HOLTMP(6)
      INTEGER   DTYP(MAXCQC), DATP(128,2), ITEMP(6), NREC, NCOL, NKEY,
     *   NC, ITRIM, NDATA, I, NTTCQ, KLOCS(NKYCQ2), KEYVAL(NKYCQ2),
     *   KEYTYP(NKYCQ2), IPOINT, IPT, IREV, JERR
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA LTCQ /'FRQSEL', 'SUBARRAY', 'FFT_SIZE', 'NO_CHAN',
     *   'SPEC_AVG', 'EDGE_FRQ', 'CHAN_BW', 'TAPER_FN', 'OVR_SAMP',
     *   'ZERO_PAD', 'FILTER', 'TIME_AVG', 'NO_BITS', 'FFT_OVLP' /
      DATA LTTCQ /'CORRELATOR PARAMETER FREQUENCY TABLE'/
      DATA LUNTCQ / 5*' ', 2*'HZ', 4*' ', 'SECONDS', 2*' '/
      DATA LKEYCQ /'NO_IF   ', 'TABREV  '/
      DATA NTTCQ / 56/
C----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = (OPCODE.EQ.'READ')
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXCQC
      IF (DOREAD) NCOL = 0
      NKEY = NKEYCQ
      NDATA = MAXCQC
      CALL FILL (NDATA, 0, CQKOLS)
      CALL FILL (NDATA, 0, CQNUMV)
C                                       Fill in types, lengths of
C                                       column data.
      IF (.NOT.DOREAD) THEN
         DTYP(CQIFQD) = TABINT + 10
         DTYP(CQISUB) = TABINT + 10
         DTYP(CQIFFT) = TABINT + 10 * NOIFCQ
         DTYP(CQINCH) = TABINT + 10 * NOIFCQ
         DTYP(CQISAV) = TABINT + 10 * NOIFCQ
         DTYP(CQDFRQ) = TABDBL + 10 * NOIFCQ
         DTYP(CQDCBW) = TABDBL + 10 * NOIFCQ
         DTYP(CQHTAP) = TABHOL + 8 * 10 * NOIFCQ
         DTYP(CQIOVS) = TABINT + 10 * NOIFCQ
         DTYP(CQIZPD) = TABINT + 10 * NOIFCQ
         DTYP(CQIFLT) = TABINT + 10 * NOIFCQ
         DTYP(CQRTAV) = TABFLT + 10 * NOIFCQ
         DTYP(CQIBIT) = TABINT + 10 * NOIFCQ
         DTYP(CQIOVL) = TABINT + 10 * NOIFCQ
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CQ', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CQINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get no. of records
      ICQRNO = BUFFER(5) + 1
      IF (DOREAD) ICQRNO = 1
      NKEY = NKEYCQ
C                                       File created; initialize
      IF (NEWFIL) THEN
C                                       Column labels
         DO 40 I = 1, NCOL
            CALL CHR2H (24, LTCQ(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CQINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, LUNTCQ(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CQINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in table title
         CALL CHR2H (NTTCQ, LTTCQ, 1, BUFFER(101))
C                                       Write keywords if table created
         IPOINT = 1
         IPT = 1
C                                       No. IF's
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = NOIFCQ
         IPOINT = IPOINT + 1
         IF (IPOINT.GT.NKYCQ2) GO TO 900
         IPT = IPT + 1
C                                       Table revision
         KLOCS(IPT) = IPOINT
         KEYTYP(IPT) = TABINT
         KEYVAL(IPOINT) = ICQREV
C                                       Write keywords
         CALL TABKEY ('WRIT', LKEYCQ, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CQINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', LKEYCQ, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CQINI', IERR)
            GO TO 990
            END IF
C                                       No. IF's
         IPT = 1
         NOIFCQ = 0
         IPOINT = KLOCS(IPT)
         IF (IPOINT.GT.0) NOIFCQ = KEYVAL(IPOINT)
         IPT = IPT + 1
C                                       Table revision
         IREV = 1
         IPOINT = KLOCS(IPT)
         IF (IPOINT.GT.0) IREV = KEYVAL(IPOINT)
C                                       Abort if unrecognized revision
         IF (IREV.GT.ICQREV) THEN
            IERR = 10
            MSGTXT = 'CQINI: UNRECOGNIZED CQ TABLE FORMAT; ' //
     *        'USE LATER AIPS VERSION'
            CALL MSGWRT (6)
            GO TO 990
            END IF
         END IF
C                                       Get array indices; Prevent
C                                       problems with FNDCOL - close
C                                       to flush buffers then re-open
      CALL TABIO ('CLOS', 0, 0, BUFFER, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CQINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
C                                       Re-open
      CALL TABINI (OPCODE, 'CQ', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CQINI', IERR)
         GO TO 990
         END IF
C
      CALL FNDCOL (NDATA, LTCQ, 24, .TRUE., BUFFER, CQKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = CQKOLS(I)
         IF (IPOINT.GT.0) THEN
            CQKOLS(I) = DATP(IPOINT,1)
            CQNUMV(I) = DATP(IPOINT,2) / 10
            IF (CQNUMV(I).LE.0) THEN
               NC = ITRIM (LTCQ(I))
               WRITE (MSGTXT,1100) LTCQ(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CQKOLS(I) = -1
            CQNUMV(I) = 0
            NC = ITRIM (LTCQ(I))
            WRITE (MSGTXT,1101) LTCQ(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       NKYCQ2 error
 900  WRITE (MSGTXT,1900) IPOINT, NKYCQ2
      IERR = 11
      CALL MSGWRT (6)
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (6)
C
 999  RETURN
C----------------------------------------------------------------------
 1100 FORMAT ('CQINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CQINI: ''',A,''' COLUMN NOT FOUND')
 1900 FORMAT ('CQINI: POINTER VALUE: ',I4,' > MAXIMUM: ',I4)
 1990 FORMAT ('CQINI: ERROR INITIALIZING CORR FREQUENCY TABLE FOR ',A4)
      END
