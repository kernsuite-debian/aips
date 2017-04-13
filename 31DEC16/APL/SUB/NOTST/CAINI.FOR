      SUBROUTINE CAINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! creates and intializes a CAlibration table.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006
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
C   Creates and initializes antenna characteristics tables.
C   Inputs:
C      OPCODE         C*4  Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      BUFFER(512)    I    I/O buffer and related storage, also defines
C                          file if open.
C      DISK           I    Disk to use.
C      CNO            I    Catalog slot number
C      CATBLK(256)    I    Catalog header block.
C      LUN            I    Logical unit number to use
C   In/out:
C      VER            I    CA file version
C   Input/output via common (file keywords):
C      OBSCOD         C*8  Observing code.
C      NOSTKD         I    No. polzns. in data.
C      STK1           I    First Stokes' parameter in data.
C      NOBAND         I    Number of bands (=IFs) in data.
C      NOCHAN         I    Number of spectral channels in data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      NOANT          I    Number of antennas for which there is info.
C      NOPOL          I    Number of polarisations (R or L) in the
C                                                               table.
C
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented,
C                            BJ, Feb 19, 1991
C
C      CANUMV(MAXCAC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output:
C      ICARNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      CAKOLS(MAXCAC) I    The column pointer array in order:
C                          TIME, TIME INTERVAL, SOURCE ID, ANTENNA NO.,
C                          ARRAY, FREQID,
C                            --- Polarisation 1 ---
C                          TSYS1, TANT1, SENSITIVITY1, PHASE1, RATE1,
C                          DELAY1, REAL1, IMAG1, WEIGHT1, REFANT1,
C                           + these if there are two polarisations
C                           e.g. R,L or X,Y
C                            --- Polarisation 2 ---
C                          TSYS2, TANT2, SENSITIVITY2, PHASE2, RATE2,
C                          DELAY2, REAL2, IMAG2, WEIGHT2, REFANT2,
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
C                                       Input variables
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IERR
C                                       Local variables
      INTEGER   I, ITEMP(6), JERR, NTT, ITRIM, NC
      LOGICAL   T, DOREAD, NEWFIL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (HOLTMP, ITEMP)
      INCLUDE 'INCS:DCAV.INC'
      CHARACTER  KEYW(NKEYCA)*8, TITLE(MAXCAC)*24, UNITS(MAXCAC)*8,
     *   TTITLE*56
C                                       CA and generic data statements
      DATA NTT /18/
      DATA TTITLE /'CALIBRATION TABLE'/
C                                       Table column titles
      DATA TITLE /'TIME', 'TIME_INTERVAL', 'SOURCE_ID', 'ANTENNA_NO',
     *   'ARRAY', 'FREQID',
C                                       Polarisation 1
     *   'TSYS_1', 'TANT_1', 'SENSITIVITY_1', 'PHASE_1',
     *   'RATE_1', 'DELAY_1', 'REAL_1', 'IMAG_1', 'WEIGHT_1',
     *   'REFANT_1',
C                                       Polarisation 2
     *   'TSYS_2', 'TANT_2', 'SENSITIVITY_2', 'PHASE_2',
     *   'RATE_2', 'DELAY_2', 'REAL_2', 'IMAG_2', 'WEIGHT_2',
     *   'REFANT_2'/
C
C                                       Units of table columns
      DATA UNITS /2*'DAYS', 4*' ',
C                                       Polarisation 1
     *   2*'KELVIN', 'K/JY', 'RADIANS', 'SEC/SEC', 'SECONDS',
     *   4*' ',
C                                       Polarisation 2
     *   2*'KELVIN', 'K/JY', 'RADIANS', 'SEC/SEC', 'SECONDS',
     *   4*' '/
C
C                                       Header keywords
      DATA KEYW /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'NO_BAND ',
     *   'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL',
     *   'NO_ANT  ', 'NO_POL  ', 'TABREV  ' /
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXCAC
      NKEY = NKEYCA
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(KTIM) = TABDBL  + 10
         DTYP(KINT) = TABFLT  + 10
         DTYP(KSID) = TABINT + 10
         DTYP(KANT) = TABINT + 10
         DTYP(KARR) = TABINT + 10
         DTYP(KFID) = TABINT + 10
         DTYP(KTA1) = TABFLT  + 10 * NOBAND
         DTYP(KTS1) = TABFLT  + 10 * NOBAND
         DTYP(KSE1) = TABFLT  + 10 * NOBAND
         DTYP(KPH1) = TABFLT  + 10 * NOBAND
         DTYP(KRT1) = TABFLT  + 10 * NOBAND
         DTYP(KDY1) = TABFLT  + 10 * NOBAND
         DTYP(KRL1) = TABFLT  + 10 * NOBAND
         DTYP(KIM1) = TABFLT  + 10 * NOBAND
         DTYP(KWT1) = TABFLT  + 10 * NOBAND
         DTYP(KRF1) = TABINT + 10 * NOBAND
         DTYP(KTA2) = TABFLT  + 10 * NOBAND
         DTYP(KTS2) = TABFLT  + 10 * NOBAND
         DTYP(KSE2) = TABFLT  + 10 * NOBAND
         DTYP(KPH2) = TABFLT  + 10 * NOBAND
         DTYP(KRT2) = TABFLT  + 10 * NOBAND
         DTYP(KDY2) = TABFLT  + 10 * NOBAND
         DTYP(KRL2) = TABFLT  + 10 * NOBAND
         DTYP(KIM2) = TABFLT  + 10 * NOBAND
         DTYP(KWT2) = TABFLT  + 10 * NOBAND
         DTYP(KRF2) = TABINT + 10 * NOBAND
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CA', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CAINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ICARNO = BUFFER(5) + 1
      IF (DOREAD) ICARNO = 1
      NKEY = NKEYCA
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CAINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CAINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         CALL CAIO ('WRIT', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'CAIO', 'CAINI', IERR)
            GO TO 990
            END IF
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR (OPCODE, 'TABKEY', 'CAINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CAINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
         CALL CAIO ('READ', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'CAIO', 'CAINI', IERR)
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CAINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CA', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CAINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXCAC, TITLE, 24, T, BUFFER, CAKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXCAC
         IPOINT = CAKOLS(I)
         IF (IPOINT.GT.0) THEN
            CAKOLS(I) = DATP(IPOINT,1)
            CANUMV(I) = DATP(IPOINT,2) / 10
            IF (CANUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CAKOLS(I) = -1
            CANUMV(I) = 0
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
 1100 FORMAT ('CAINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CAINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CAINI: ERROR INITIALIZING CALIBRATION (CA) TABLE FOR ',
     *   A4)
      END


