      SUBROUTINE CALINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
C-----------------------------------------------------------------------
C! Creates/opens/initializes calibration (CL) table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006, 2009
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
C   Creates and initializes calibration extension tables.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   CL file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C    Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of IFs per pair
C     NUMIF        I    Number of IF pairs
C     NTERM        I    Number of terms in model polynomial.
C     GMMOD        R    Mean gain modulus
C     CLNUMV(MAXCLC) I   Element count in each column.
C                        before the creation of a new CL table.
C    Output:
C     ICLRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     CLKOLS(MAXCLC) I   The column pointer array in order defined by
C                        the parameters in PCLTAB.INC
C     CLNUMV(MAXCLC) I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICLRNO,
     *   CLKOLS(*), CLNUMV(*), NUMANT, NUMPOL, NUMIF, NTERM, IERR
      REAL      GMMOD
c
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE1(11)*24, TITLE2(11)*24, TITLE3(11)*24,
     *   TITLE(MAXCLC)*24, UNITS(MAXCLC)*8, KEYW(6)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXCLC), NDATA,
     *   KLOCS(6), KEYVAL(10), IPOINT, JERR, J, KEYTYP(6), I, REVNO,
     *   MSGSAV, NC, ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      REAL      KEYR(10)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TITLE(1), TITLE1), (TITLE(12), TITLE2),
     *   (TITLE(23), TITLE3)
      EQUIVALENCE (KEYVAL, KEYR)
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE CALIBRATION TABLE '/
C                                       Values >10 are scalar
      DATA DTYP /11,12,4*14,12,1,2,12,12, 5*12,5*2,4, 5*12,5*2,4/
C                                       NOTE: Change PCLTAB.INC when
C                                       changing the table columns.
      DATA TITLE1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA TITLE2 /'MBDELAY1                ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                '/
      DATA TITLE3 /'MBDELAY2                ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ', 'NO_TERM ',
     *   'MGMOD  ', 'REVISION'/
      DATA UNITS /'DAYS    ','DAYS    ', 4*'        ', 'RAD/M**2',
     *   'SECONDS ', 'HZ      ', 'SECONDS ', 'SEC/SEC ',
     *   'SECONDS', 'SECONDS ', 'SEC/SEC ', 'SEC/M**2', 'S/S/M**2',
     *   2*'        ', 'SEC/SEC ', 'SECONDS ' ,2*'        ',
     *   'SECONDS', 'SECONDS ', 'SEC/SEC ', 'SEC/M**2', 'S/S/M**2',
     *   2*'        ', 'SEC/SEC ', 'SECONDS ', 2*'        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 11 + NUMPOL * 11
      IF (DOREAD) NCOL = 0
      NKEY = 6
      NDATA = MAXCLC
      CALL FILL (NDATA, 0, CLKOLS)
      CALL FILL (NDATA, 0, CLNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
C                                       Special cases: GEODELAY
         DATP(CLDDEL,2) = DTYP(CLDDEL) + 10 * NTERM
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CALINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ICLRNO = BUFFER(5) + 1
      IF (DOREAD) ICLRNO = 1
      NKEY = 6
      MSGSAV = MSGSUP
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CALINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CALINI', IERR)
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
C                                       No. polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. model poly. terms
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NTERM
C                                       Gain modulus
         KLOCS(5) = 5
         KEYTYP(5) = 1
         KEYVAD = GMMOD
         CALL RCOPY (NWDPDP, KEYVAD, KEYR(5))
         IPOINT = 5 + NWDPDP
C                                       Revision number from PCLTAB.INC
         REVNO = CLREV
         KLOCS(6) = IPOINT
         KEYTYP(6) = 4
         KEYVAL(IPOINT) = REVNO
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CALINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CALINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. poly. model terms
         IPOINT = KLOCS(4)
         NTERM = 0
         IF (IPOINT.GT.0) NTERM = KEYVAL(IPOINT)
C                                       Gain modulus
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(5).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
               GMMOD = KEYVAD
            ELSE
               GMMOD = KEYR(IPOINT)
               END IF
            END IF
C                                       Revision level
         IPOINT = KLOCS(6)
         REVNO = -1
         IF (IPOINT.GT.0) REVNO = KEYVAL(IPOINT)
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CALINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CL', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CALINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXCLC, TITLE, 24, T, BUFFER, CLKOLS, JERR)
C                                      Get array indices and no. values
      NCOL = 11 + NUMPOL * 11
      DO 150 I = 1,NCOL
         IPOINT = CLKOLS(I)
         IF (IPOINT.GT.0) THEN
            CLKOLS(I) = DATP(IPOINT,1)
            CLNUMV(I) = DATP(IPOINT,2) / 10
            IF (CLNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CLKOLS(I) = -1
            CLNUMV(I) = 0
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
 1100 FORMAT ('CALINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CALINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CALINI: ERROR INITIALIZING CALIBRATION TABLE FOR ',A4)
      END
