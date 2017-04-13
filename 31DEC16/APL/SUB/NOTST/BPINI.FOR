      SUBROUTINE BPINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *   NUMSHF, LOWSHF, DELSHF, LBPTYP, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize bandpass (BP) table
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006-2007
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
C   Creates and initializes bandpass (BP) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        BP file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      NUMANT   I        Number of antennas
C      NUMPOL   I        Number of polarizations.
C      NUMIF    I        Number of IFs
C      NUMFRQ   I        Number of frequency channels
C      BCHAN    I        Start channel number
C      NUMSHF   I        Use this for other purposes than originally
C                        intended. If NUMSHF = 1 BP entries are from
C                        cross-power data, if 2 are from total power,
C                        if 3 are a mixture, anything else then type
C                        is unknown and will assume cross-power.
C      LOWSHF   R        Most negative shift
C      DELSHF   R        Shift increment
C      LBPTYP   C*8      BP type: ' ' => standard BP table,
C                        'CHEBSHEV' => Chebyshev polynomial coeff.
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IBPRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      BPKOLS   I(MAXBPC)   The column pointer array in order:
C                        TIME, INTERVAL, SOURID,
C                        SUBARRAY, ANTENNA,
C                        BANDW (of individual channel),
C                        CHN_SHIFT (per IF)
C                        FREQ. ID,
C                        REFANT1, WT1, REAL1, IMAG1,
C                        Following used if 2 polarizations per IF
C                        REFANT2, WT2, REAL2, IMAG2.
C     BPNUMV    I(MAXBPC)   Element count in each column.
C     IERR      I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4, LBPTYP*8
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IBPRNO,
     *   BPKOLS(MAXBPC), BPNUMV(MAXBPC), NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   BCHAN, NUMSHF, IERR
      REAL      LOWSHF, DELSHF
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXBPC)*24, UNITS(MAXBPC)*8, KEYW(9)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXBPC), NDATA,
     *   KLOCS(9), KEYVAL(12), KEYTYP(9), IPOINT, MSGSAV, I, NC, JERR,
     *   ITRIM
      REAL      KEYVAR(12)
      DOUBLE PRECISION KEYVAD
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVAR)
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE BANDPASS TABLE            '/
      DATA DTYP /11, 12, 3*14, 12, 1, 14, 14, 3*2, 14, 3*2/
      DATA TITLE /'TIME ', 'INTERVAL ', 'SOURCE ID ', 'SUBARRAY ',
     *   'ANTENNA ', 'BANDWIDTH ', 'CHN_SHIFT ', 'FREQ ID ',
     *   'REFANT 1 ', 'WEIGHT 1', 'REAL 1 ', 'IMAG 1 ',
     *   'REFANT 2 ', 'WEIGHT 2', 'REAL 2 ', 'IMAG 2 '/
      DATA KEYW /'NO_ANT', 'NO_POL', 'NO_IF', 'NO_CHAN', 'STRT_CHN',
     *   'NO_SHFTS', 'LOW_SHFT', 'SHFT_INC', 'BP_TYPE' /
      DATA UNITS /'DAYS ', 'DAYS ',3*' ','HZ ',10*' '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = 9
      NDATA = MAXBPC
      CALL FILL (NDATA, 0, BPKOLS)
      CALL FILL (NDATA, 0, BPNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         NCOL = 8 + NUMPOL * 4
         CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Chan. shift type
         DATP(7,2) = DTYP(7) + 10*NUMIF
C                                       Correct for real/imag
         DATP(10,2) = DTYP(10) + 10*NUMIF
         DATP(11,2) = DTYP(11) + 10*NUMFRQ*NUMIF
         DATP(12,2) = DTYP(12) + 10*NUMFRQ*NUMIF
         DATP(14,2) = DTYP(14) + 10*NUMIF
         DATP(15,2) = DTYP(15) + 10*NUMFRQ*NUMIF
         DATP(16,2) = DTYP(16) + 10*NUMFRQ*NUMIF
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'BP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BPINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      IBPRNO = BUFFER(5) + 1
      IF (DOREAD) IBPRNO = 1
      NKEY = 9
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BPINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'BPINI', IERR)
               GO TO 990
               END IF
 10         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. Polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. CHAN
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NUMFRQ
C                                       Start CHAN
         KLOCS(5) = 5
         KEYTYP(5) = 4
         KEYVAL(5) = BCHAN
C                                       # shift entries
         KLOCS(6) = 6
         KEYTYP(6) = 4
         KEYVAL(6) = NUMSHF
C                                       most negative freq shift
         KLOCS(7) = 7
         KEYTYP(7) = 2
         KEYVAD = LOWSHF
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(7))
         IPOINT = 7 + NWDPDP
C                                       shift increment
         KLOCS(8) = IPOINT
         KEYTYP(8) = 1
         KEYVAD = DELSHF
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       BP table type
         KLOCS(9) = IPOINT
         KEYTYP(9) = 3
         CALL CHR2H (8, LBPTYP, 1, KEYVAL(IPOINT))
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'BPINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'BPINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. polarizations
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IFs
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. CHAN
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) NUMFRQ = KEYVAL(IPOINT)
C                                       Start chan
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) BCHAN = KEYVAL(IPOINT)
C                                       # shifts
         IPOINT = KLOCS(6)
         IF (IPOINT.GT.0) NUMSHF = KEYVAL(IPOINT)
C                                       most negative shift
         IPOINT = KLOCS(7)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(7).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVAL(IPOINT), KEYVAD)
               LOWSHF = KEYVAD
            ELSE
               LOWSHF = KEYVAR(IPOINT)
               END IF
            END IF
C                                       Shift increment
         IPOINT = KLOCS(8)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(8).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVAL(IPOINT), KEYVAD)
               DELSHF = KEYVAD
            ELSE
               DELSHF = KEYVAR(IPOINT)
               END IF
            END IF
C                                       BP table type
         IPOINT = KLOCS(9)
         LBPTYP = ' '
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), LBPTYP)
         END IF
      IERR = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'BPINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'BP', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'BPINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, BPKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = BPKOLS(I)
         IF (IPOINT.GT.0) THEN
            BPKOLS(I) = DATP(IPOINT,1)
            BPNUMV(I) = DATP(IPOINT,2) / 10
            IF (BPNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            BPKOLS(I) = -1
            BPNUMV(I) = 0
            IF ((NUMPOL.GT.1) .OR. (I.LE.NDATA-4)) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1101) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BPINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('BPINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('BPINI: ERROR INITIALIZING BANDPASS TABLE FOR ',A)
      END
