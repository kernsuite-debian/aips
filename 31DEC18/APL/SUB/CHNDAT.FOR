      SUBROUTINE CHNDAT (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
C-----------------------------------------------------------------------
C! Creates/Opens/Reads/Writes/Closes an IF table.
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2012, 2014
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
C   Routine to create/fill/read CH/FQ extension tables.
C   We are phasing out CH tables, so this routine will read them, but
C   will only write FQ tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C      FREQID   I        Frequency ID #, if FQ tables exists.
C                        If OPCODE='READ' and FREQID .le. 0, then
C                        if there is only one row in the FQ table,
C                        that row is returned; if there are multiple
C                        rows an error message is returned.
C   Input/Output:
C      VER      I        CH file version
C      NIF      I        Number of IFs.
C      FOFF     D(*)     Frequency offset in Hz from ref. freq.
C                           True = reference + offset.
C      ISBAND   I(*)     Sideband of each IF.
C                        -1 => 0 video freq. is high freq. end
C                         1 => 0 video freq. is low freq. end
C      FINC     R(*)     Channel bandwidth in Hz of each IF
C   Output:
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error, -1 => tried to create/write an FQ table
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, BNDCOD(*)*(*)
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, NIF,
     *   ISBAND(*), FREQID, IERR
      DOUBLE PRECISION FOFF(*)
      REAL      FINC(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHIF*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, CHKOLS(3), IAXFRQ,
     *   DTYP(3), NDATA, RECI(8), JERR, IFOFF, NOKOL, OFFKOL, SBKOL,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO, NUMFQE, FQID,
     *   FQSID(MAXIF), ICHRNO, I, J, CATEQU(256), FREQTM
      LOGICAL   DOREAD, FQEXIS, TABLE, FITASC, TAOPEN
      REAL      FQCHB(MAXIF), FQTBW(MAXIF), CATR(128)
      DOUBLE PRECISION  RECD(4), FQFRQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CHKOLS(1), NOKOL), (CHKOLS(2), OFFKOL),
     *   (CHKOLS(3), SBKOL),  (RECI, RECD)
      EQUIVALENCE (CATEQU, CATR)
      DATA CHIF /'IF'/
      DATA NDATA /3/
      DATA DTYP /14,11,14/
C-----------------------------------------------------------------------
      IERR = 0
      CALL COPY (256, CATBLK, CATEQU)
      FREQTM = FREQID
      TAOPEN = .FALSE.
C                                        Find Freq axis
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), IAXFRQ,
     *   IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'CHNDAT: COULD NOT FIND FREQ AXIS'
         GO TO 990
         END IF
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Check for existence of FQ table
      CALL ISTAB ('FQ', DISK, CNO, VER, LUN, BUFFER, TABLE, FQEXIS,
     *   FITASC, JERR)
      IF (FQEXIS .AND. DOREAD) GO TO 300
C                                       Read CH table?
      IF (DOREAD) THEN
C                                       Look for IF axis; if none there
C                                       better not be a CH table. Will
C                                       AIPS function without an IF axis
         CALL AXEFND (4, CHIF, CATBLK(KIDIM), CATBLK(KHCTP), IFOFF,
     *      JERR)
         IF (JERR.NE.0) THEN
C                                       No IF axis.
            BUFFER(5) = 0
            NIF = 1
            FOFF(1) = 0.0D0
            ISBAND(1) = 1
            FINC(1) = CATR(KRCIC+JLOCF)
            GO TO 999
            END IF
C                                       1 IF only, use header values
         IF (CATBLK(KINAX+IFOFF).LE.1) THEN
            BUFFER(5) = 1
            NIF = 1
            FOFF(1) = 0.0D0
            ISBAND(1) = 1
            FINC(1) = CATR(KRCIC+IAXFRQ)
            GO TO 999
            END IF
C                                       Open file
         NREC = 20
         NCOL = NDATA
         NKEY = 0
C                                       Fill in types
         CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Create/open CH table
         CALL TABINI ('READ', 'CH', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *      NREC, NCOL, DATP, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         TAOPEN = .TRUE.
C                                       Get number of Channels
         NIF = BUFFER(5)
C                                       Get array indices
         DO 150 I = 1,NDATA
            CHKOLS(I) = DATP(I,1)
 150        CONTINUE
C                                       Read table entries
         DO 200 I = 1,NIF
            ICHRNO = I
            CALL TABIO (OPCODE, 0, ICHRNO, RECI, BUFFER, IERR)
            IF (IERR.GT.0) GO TO 995
            IF (IERR.LT.0) GO TO 200
            J = RECI(NOKOL)
            FOFF(J) = RECD(OFFKOL)
C                                       No value in CH table
            FINC(J) = CATR(KRCIC+IAXFRQ)
            ISBAND(J) = 1
            BNDCOD(J) = ' '
 200        CONTINUE
C                                       Close
         CALL TABIO ('CLOS', 0, ICHRNO, RECI, BUFFER, IERR)
         TAOPEN = .FALSE.
         GO TO 995
         END IF
C                                       Create/write FQ file
C                                       Open file
      CALL FQINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, IFQRNO,
     *   FQKOLS, FQNUMV, NIF, IERR)
      IF (IERR.GT.0) GO TO 999
      TAOPEN = .TRUE.
C                                       Write entry
      DO 250 I = 1,NIF
         FQFRQ(I) = FOFF(I)
         IF (ISBAND(I).EQ.0) ISBAND(I) = 1
         FQSID(I) = ISBAND(I)
         FQCHB(I) = FINC(I)
         FQTBW(I) = ABS(FINC(I) * CATBLK(KINAX+IAXFRQ))
 250     CONTINUE
      IF (FREQID.LE.0) FREQTM = 1
C
      CALL TABFQ ('WRIT', BUFFER, IFQRNO, FQKOLS, FQNUMV, NIF, FREQTM,
     *   FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, IERR)
      TAOPEN = .FALSE.
      GO TO 999
C                                       FQ reading section
 300  CALL FQINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, IFQRNO,
     *   FQKOLS, FQNUMV, NIF, IERR)
      IF (IERR.NE.0) GO TO 999
      TAOPEN = .TRUE.
      NUMFQE = BUFFER(5)
C                                       Ambiguous request.  GIve
C                                       them nothing.
      IF ((FREQID.LE.0) .AND. (NUMFQE.NE.1)) THEN
         WRITE (MSGTXT,1000) FREQID
         IERR = 1
         GO TO 990
         END IF
C                                       Loop and read
      DO 350 I = 1,NUMFQE
         IFQRNO = I
         CALL TABFQ ('READ', BUFFER, IFQRNO, FQKOLS, FQNUMV, NIF, FQID,
     *      FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IERR)
         IF (IERR.NE.0) GO TO 995
         IF ((FQID.EQ.FREQTM) .OR. (FREQTM.LE.0)) GO TO 400
 350     CONTINUE
C                                       Tidy up
      CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, JERR)
      TAOPEN = .FALSE.
C                                       No match
      WRITE (MSGTXT, 2000) FREQTM
      IERR = 1
C                                       Make sure they have nothing
C                                       useful with this error
      NIF = 1
      FOFF(1) = 0.0D0
      ISBAND(1) = 0
      FINC(1) = 0.0
      BNDCOD(1) = ' '
      GO TO 990
C                                       Fill in the good values
 400  DO 410 I = 1,NIF
         FOFF(I) = FQFRQ(I)
         FINC(I) = FQCHB(I)
         ISBAND(I) = FQSID(I)
         IF (ISBAND(I).EQ.0) ISBAND(I) = 1
 410     CONTINUE
      CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, IERR)
      TAOPEN = .FALSE.
      GO TO 999
C                                        Error
 990  CALL MSGWRT (6)
C
 995  IF (TAOPEN) CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHNDAT: FREQID = ',I3,' INVALID ON READ')
 2000 FORMAT ('CHNDAT: FREQID = ',I3,' NOT IN FQ TABLE')
      END
