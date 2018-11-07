      SUBROUTINE GAINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IGARNO, GAKOLS, GANUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, IERR)
C-----------------------------------------------------------------------
C! Creates and initializes gain (GA) extension tables.
C# EXT-util Calibration Obselete
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
C   Creates and initializes gain extension tables.
C   Inputs:
C      OPCODE   C*4     Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      BUFFER   I(512)  I/O buffer and related storage, also defines
C                       file if open.
C      DISK     I       Disk to use.
C      CNO      I       Catalog slot number
C      VER      I       GA file version
C      CATBLK   I(256)  Catalog header block.
C      LUN      I       Logical unit number to use
C   Input (create) / output (pre-existing)
C      NUMANT   I       Number of antennas
C      NUMPOL   I       Number of IFs per group
C      NUMIF    I       Number of IF groups
C      NUMNOD   I       Number of interpolation nodes. Will handle
C                       up to 25 interpolation nodes.
C      GMMOD    R       Mean gain modulus
C      RANOD    R(*)    RA offset of interpolation nodes (deg.)
C      DECNOD   R(*)    Dec. offset of interpolation nodes (deg.)
C   Output:
C      IGARNO   I       Next scan number, start of the file if 'READ',
C                       the last+1 if WRITE
C      GAKOLS   I(18)   The column pointer array in order, TIME,
C                       TIME INT., SOURCE ID., ANTENNA NO., SUBARRAY,
C                       NODE NO.,
C                       REAL1, IMAG1, DELAY1, RATE1, WEIGHT1, REFANT 1,
C                       Following used if 2 polarizations per IF
C                       REAL2, IMAG2, DELAY2, RATE2, WEIGHT2, REFANT 2
C      GANUMV   I(18)   Element count in each column.
C      IERR     I       Return error code, 0 => OK,
C                          else TABINI or TABIO error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, TTITLE*56, TITLE(18)*24, UNITS(18)*8,
     *   KEYW(55)*8
      HOLLERITH HOLTMP(6)
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IERR,
     *   NKEY, NREC, DATP(128,2), NCOL, GAKOLS(18), GANUMV(18), NTT,
     *   DTYP(18), NDATA, KLOCS(55), KEYVAL(120), KEYTYP(55), IPOINT, J,
     *   JNUM, NUMANT, NUMPOL, NUMIF, NUMNOD, ITRIM, NC, JERR
      LOGICAL   DOREAD, NEWFIL, T
      INTEGER   IGARNO, I
      REAL      GMMOD, RANOD(25), DECNOD(25), KEYVR(120)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVR)
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE GAIN TABLE '/
      DATA NDATA /18/
      DATA DTYP /11,12,14,14,14,14,5*2,4,5*2,4/
      DATA TITLE /'TIME ', 'TIME INTERVAL ', 'SOURCE ID ',
     *   'ANTENNA NO. ', 'SUBARRAY ', 'NODE NO. ', 'REAL1 ', 'IMAG1 ',
     *   'DELAY 1 ', 'RATE 1 ', 'WEIGHT 1 ', 'REFANT 1 ', 'REAL2 ',
     *   'IMAG2 ', 'DELAY 2 ', 'RATE 2 ', 'WEIGHT 2 ', 'REFANT 2 '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ',
     *   'NO_NODES', 'MGMOD   ',
     *   'RA_OFF1 ', 'DEC_OFF1', 'RA_OFF2 ','DEC_OFF2',
     *   'RA_OFF3 ', 'DEC_OFF3', 'RA_OFF4 ','DEC_OFF4',
     *   'RA_OFF5 ', 'DEC_OFF5', 'RA_OFF6 ','DEC_OFF6',
     *   'RA_OFF7 ', 'DEC_OFF7', 'RA_OFF8 ','DEC_OFF8',
     *   'RA_OFF9 ', 'DEC_OFF9', 'RA_OFF10','DEC_OF10',
     *   'RA_OFF11', 'DEC_OF11', 'RA_OFF12','DEC_OF12',
     *   'RA_OFF13', 'DEC_OF13', 'RA_OFF14','DEC_OF14',
     *   'RA_OFF15', 'DEC_OF15', 'RA_OFF16','DEC_OF16',
     *   'RA_OFF17', 'DEC_OF17', 'RA_OFF18','DEC_OF18',
     *   'RA_OFF19', 'DEC_OF19', 'RA_OFF20','DEC_OF20',
     *   'RA_OFF21', 'DEC_OF21', 'RA_OFF22','DEC_OF22',
     *   'RA_OFF23', 'DEC_OF23', 'RA_OFF24','DEC_OF24',
     *   'RA_OFF25', 'DEC_OF25'/
      DATA UNITS /'DAYS    ','DAYS    ', 4*'        ',
     *    2*'        ', 'SECONDS ','SEC/SEC', 2*'        ',
     *    2*'        ', 'SECONDS ','SEC/SEC', 2*'        '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 6 + NUMPOL * 6
      NKEY = 5 + NUMNOD * 2
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 7,18
            DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'GA', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GAINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of scans
      IGARNO = BUFFER(5) + 1
      IF (DOREAD) IGARNO = 1
      NKEY = 5 + NUMNOD * 2
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GAINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'GAINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. IFs per group.
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IF groups.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. interpolation nodes.
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NUMNOD
C                                       Gain modulus
         KLOCS(5) = 5
         KEYTYP(5) = 2
         KEYVAD = GMMOD
         CALL RCOPY (NWDPDP, KEYVAD, KEYVR(5))
         IPOINT = 5 + NWDPDP
C                                       Interpolation nodes
         JNUM = 6
         DO 110 J = 1,NUMNOD
            KLOCS(JNUM) = IPOINT
            KEYTYP(JNUM) = 1
            KEYVAD = RANOD(J)
            CALL RCOPY (NWDPDP, KEYVAD, KEYVR(IPOINT))
            IPOINT = IPOINT + NWDPDP
            JNUM = JNUM + 1
            KLOCS(JNUM) = IPOINT
            KEYTYP(JNUM) = 1
            KEYVAD = DECNOD(J)
            CALL RCOPY (NWDPDP, KEYVAD, KEYVR(IPOINT))
            IPOINT = IPOINT + NWDPDP
            JNUM = JNUM + 1
 110        CONTINUE
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'GAINI', IERR)
            GO TO 990
            END IF
C                                       READ
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'GAINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per group.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF groups.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. interpolation nodes.
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) NUMNOD = KEYVAL(IPOINT)
C                                       Gain modulus
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(5).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
               GMMOD = KEYVAD
            ELSE
               GMMOD = KEYVR(IPOINT)
               END IF
            END IF
C                                       Interpolation nodes
         IF (NUMNOD.GT.0) THEN
            JNUM = 6
            DO 130 J = 1,NUMNOD
               IPOINT = KLOCS(JNUM)
               IF (IPOINT.GT.0) THEN
                  IF (KEYTYP(JNUM).EQ.1) THEN
                     CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
                     RANOD(J) = KEYVAD
                  ELSE
                     RANOD(J) = KEYVR(IPOINT)
                     END IF
                  END IF
               JNUM = JNUM + 1
               IPOINT = KLOCS(JNUM)
               IF (IPOINT.GT.0) THEN
                  IF (KEYTYP(JNUM).EQ.1) THEN
                     CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
                     DECNOD(J) = KEYVAD
                  ELSE
                     DECNOD(J) = KEYVR(IPOINT)
                     END IF
                  END IF
               JNUM = JNUM + 1
 130           CONTINUE
            END IF
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'GAINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'GA', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'GAINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, T, BUFFER, GAKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = GAKOLS(I)
         IF (IPOINT.GT.0) THEN
            GAKOLS(I) = DATP(IPOINT,1)
            GANUMV(I) = DATP(IPOINT,2) / 10
            IF (GANUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            GAKOLS(I) = -1
            GANUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GAINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('GAINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('GAINI: ERROR INITIALIZING GAIN TABLE FOR ',A)
      END
