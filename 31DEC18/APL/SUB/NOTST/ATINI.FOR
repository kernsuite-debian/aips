      SUBROUTINE ATINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! creates and intializes an antenna characteristcs table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2009
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
C      VER            I    AT file version
C   Output
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C   Input (create) / output (pre-existing) in common (file keywords):
C      NOPCAL         I    # polzn calibration constants
C      POLTYP         C*8  Feed polzn parameterization:
C                          '        ' => no feed parms entered
C                          'APPROX  ' => linear approx.
C                          'ORI-ELP ' => orientation-ellipticity
C                          'X-Y LIN ' => linear approx for X-Y data
C      ATNUMV(MAXATC) I    Element count in each column. On input only
C                          used if the file is created.
C      OBSCOD         C*8  Observing code
C      NOSTKD         I    No polzns in the data
C      STK1           I    First Stokes parameter in the data
C      NOBAND         I    The number of bands (IF's) in the data.
C      NOCHAN         I    The number of spectral channels in the data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented, PJD, Feb 1, 1991
C   Output in common:
C      IATRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      ATKOLS(MAXATC) I    The column pointer array in order:
C                          TIME, TIME INTERVAL, ANNAME, ANTENNA NO.,
C                          ARRAY, FREQID, NO_LEVELS, POLTYA, POLAA,
C                          POLCALA, POLTYB, POLAB, POLCALB.
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
      EQUIVALENCE (HOLTMP, ITEMP)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DATV.INC'
      CHARACTER  KEYW(NKEYAT)*8, TITLE(MAXATC)*24, UNITS(MAXATC)*8,
     *   TTITLE*56
C                                       AT data statements
      DATA NTT /56/
      DATA TTITLE /'ANTENNA CHARACTERISTICS TABLE'/
C                                       Table column titles
      DATA TITLE /'TIME', 'TIME_INTERVAL', 'ANNAME  ', 'ANTENNA_NO',
     *   'ARRAY', 'FREQID', 'NO_LEVELS', 'POLTYA', 'POLAA', 'POLCALA',
     *   'POLTYB', 'POLAB', 'POLCALB', 'BEAMFWHM' /
C                                       Units of table columns
      DATA UNITS /'DAYS', 'DAYS', 6*' ', 'DEGREES', 2*' ', 'DEGREES',
     *   ' ' , 'DEG/M'/
C                                       Header keywords
      DATA KEYW /'NOPCAL  ', 'POLTYPE ', 'OBSCODE ',
     *   'NO_STKD ', 'STK_1   ', 'NO_BAND ', 'NO_CHAN',
     *   'REF_FREQ', 'CHAN_BW ', 'REF_PIXL', 'TABREV' /
C                                       Local data statements
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXATC
      NKEY = NKEYAT
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(KTIM) = TABDBL + 10
         DTYP(KINT) = TABFLT + 10
         DTYP(KANN) = TABHOL + 10*8
         DTYP(KANO) = TABINT + 10
         DTYP(KARR) = TABINT + 10
         DTYP(KFRE) = TABINT + 10
         DTYP(KNLV) = TABINT + 10
         DTYP(KTYA) = TABHOL + 10
         DTYP(KPAA) = TABFLT + 10 * NOBAND
         DTYP(KCLA) = TABFLT + 10 * NOBAND * NOPCAL
         DTYP(KTYB) = TABHOL + 10
         DTYP(KPAB) = TABFLT + 10 * NOBAND
         DTYP(KCLB) = TABFLT + 10 * NOBAND * NOPCAL
         DTYP(KFWH) = TABFLT + 10 * NOBAND
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'AT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ATINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IATRNO = BUFFER(5) + 1
      IF (DOREAD) IATRNO = 1
      NKEY = NKEYAT
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ATINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ATINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         CALL ATIO ('WRIT', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'ATIO', 'ATINI', IERR)
            GO TO 990
            END IF
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR (OPCODE, 'TABKEY', 'ATINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'ATINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
         CALL ATIO ('READ', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'ATIO', 'ATINI', IERR)
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
         CALL TABERR ('CLOS', 'TABIO', 'ATINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'AT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ATINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXATC, TITLE, 24, T, BUFFER, ATKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXATC
         IPOINT = ATKOLS(I)
         IF (IPOINT.GT.0) THEN
            ATKOLS(I) = DATP(IPOINT,1)
            ATNUMV(I) = DATP(IPOINT,2) / 10
            IF (ATNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF (((I.NE.10) .AND. (I.NE.13)) .OR. (NOPCAL.GT.0))
     *            CALL MSGWRT (6)
               END IF
         ELSE
            ATKOLS(I) = -1
            ATNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.NE.14) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ATINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('ATINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('ATINI: ERROR INITIALIZING ANTENNA CHARAC. TABLE FOR ',A4)
      END
