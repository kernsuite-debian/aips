      SUBROUTINE SOINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! creates and intializes a SOurce data table.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2010, 2012
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
C      VER            I    SO file version
C   Input (create) / output (pre-existing) via common(file keywords):
C      OBSCOD         C*8  Observing code.
C      NOSTKD         I    No. polzns. in data.
C      STK1           I    First Stokes' parameter in data.
C      NOBAND         I    Number of bands (=IFs) in data.
C      NOCHAN         I    Number of spectral channels in data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented, BJ, Feb 19, 1991
C      SONUMV(MAXSOC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output:
C      ISORNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      SOKOLS(MAXSOC) I    The column pointer array in order:
C                          ID. NO., SOURCE, QUAL, CALCODE, FREQID,
C                          IFLUX, QFLUX, UFLUX, VFLUX, ALPHA, FREQOFF,
C                          RAEPO, DECEPO, EQUINOX, RAAPP, DECAPP,
C                          SYSVEL, VELTYP, VELDEF, RESTFREQ, PMRA,
C                          PMDEC, PARALLAX
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGLB.INC'
C                                       Input variables
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IERR
C                                       Local variables
      INTEGER   I, ITEMP(6), JERR, NTT, NC, ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (HOLTMP, ITEMP)
      INCLUDE 'INCS:DSOV.INC'
      CHARACTER  KEYW(NKEYSO)*8, TITLE(MAXSOC)*24, UNITS(MAXSOC)*8,
     *   TTITLE*56, EPOCH*24, TITLES*24
C                                       SO and generic data statements
      DATA NTT /18/
      DATA TTITLE /'SOURCE TABLE'/
C                                       Table column titles
      DATA EPOCH /'EPOCH'/
      DATA TITLE /'SOURCE_ID','SOURCE','QUAL','CALCODE','FREQID',
     *   'IFLUX','QFLUX','UFLUX','VFLUX','ALPHA','FREQOFF','RAEPO',
     *   'DECEPO','EQUINOX','RAAPP','DECAPP','RAOBS','DECOBS',
     *   'SYSVEL','VELTYP','VELDEF','RESTFREQ','PMRA','PMDEC',
     *   'PARALLAX'/
      DATA TITLES /'ID_NO.'/
C
C                                       Units of table columns
      DATA UNITS /5*' ', 4*'JY', ' ', 'HZ', 2*'DEGREES', 'YEARS',
     *   4*'DEGREES', 'M/S', 2*' ', 'HZ', 2*'DEG/DAY',
     *   'ARCSEC'/
C
C                                       Header keywords
      DATA KEYW /'OBSCODE ', 'NO_STKD ', 'STK_1   ', 'NO_BAND ',
     *   'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ', 'REF_PIXL', 'TABREV  ' /
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXSOC
      NKEY = NKEYSO
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(KSID) = TABINT + 10
         DTYP(KNAM) = TABHOL + 10 * 16
         DTYP(KQUA) = TABINT + 10
         DTYP(KCOD) = TABHOL + 10 * 4
         DTYP(KFRQ) = TABINT + 10
         DTYP(KIFX) = TABFLT  + 10 * NOBAND
         DTYP(KQFX) = TABFLT  + 10 * NOBAND
         DTYP(KUFX) = TABFLT  + 10 * NOBAND
         DTYP(KVFX) = TABFLT  + 10 * NOBAND
         DTYP(KALF) = TABFLT  + 10 * NOBAND
         DTYP(KFQO) = TABDBL  + 10 * NOBAND
         DTYP(KRAE) = TABDBL  + 10
         DTYP(KDEE) = TABDBL  + 10
C                                       EQUINOX either double or string
         DTYP(KEQU) = TABDBL  + 10
         DTYP(KRAA) = TABDBL  + 10
         DTYP(KDEA) = TABDBL  + 10
         DTYP(KRAO) = TABDBL  + 10
         DTYP(KDEO) = TABDBL  + 10
         DTYP(KSVL) = TABDBL  + 10 * NOBAND
         DTYP(KVTP) = TABHOL + 10 * 8
         DTYP(KVDF) = TABHOL + 10 * 8
         DTYP(KRFQ) = TABDBL  + 10 * NOBAND
         DTYP(KPMR) = TABDBL  + 10
         DTYP(KPMD) = TABDBL  + 10
         DTYP(KPAR) = TABFLT  + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'SO', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SOINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ISORNO = BUFFER(5) + 1
      IF (DOREAD) ISORNO = 1
      NKEY = NKEYSO
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SOINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SOINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         CALL SOIO ('WRIT', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'SOIO', 'SOINI', IERR)
            GO TO 990
            END IF
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR (OPCODE, 'TABKEY', 'SOINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'SOINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
         CALL SOIO ('READ', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'SOIO', 'SOINI', IERR)
            GO TO 990
            END IF
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SOINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'SO', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SOINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXSOC, TITLE, 24, T, BUFFER, SOKOLS, JERR)
C                                       allow epoch for equinox
      IF (SOKOLS(14).LE.0) CALL FNDCOL (1, EPOCH, 24, T, BUFFER,
     *   SOKOLS(14), JERR)
C                                       allow ID_NO. for SOURCE_ID
      IF (SOKOLS(1).LE.0) CALL FNDCOL (1, TITLES, 24, T, BUFFER,
     *   SOKOLS(1), JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXSOC
         IPOINT = SOKOLS(I)
         IF (IPOINT.GT.0) THEN
            SOKOLS(I) = DATP(IPOINT,1)
            SONUMV(I) = DATP(IPOINT,2) / 10
            IF (SONUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            SOKOLS(I) = -1
            SONUMV(I) = 0
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
 1100 FORMAT ('SOINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('SOINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('SOINI: ERROR INITIALIZING SOURCE (SO) TABLE FOR ',A4)
      END


