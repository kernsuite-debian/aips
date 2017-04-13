      SUBROUTINE MCINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IMCRNO, MCKOLS, MCNUMV, OBSCOD, RDATE, NSTOKE, STOKE1, NUMIF,
     *   NCHAN, RFREQ, CHANBW, REFPIX, NUMPOL, FFTSIZ, OVRSMP, ZEROPD,
     *   TAPER, DELTAT, IERR)
C-----------------------------------------------------------------------
C! Creates/opens/initializes model components (MC) table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2006, 2009, 2011
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
C   Creates and initializes model components (MC) extension tables.
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
C   Input (create) / output (pre-existing):
C     OBSCOD       C*8  Observing code
C     RDATE        C*8  Reference date
C     NSTOKE       I    Number of "Stokes" channels in data
C     STOKE1       I    First "Stokes" channel number
C     NUMIF        I    Number of IFs
C     NCHAN        I    Number of channels
C     RFREQ        D    Reference frequency (Hz)
C     CHANBW       R    Channel bandwidth (Hz)
C     REFPIX       R    Reference pixel for frequency
C     NUMPOL       I    Number of polarizations in table
C     FFTSIZ       I    FFT size
C     OVRSMP       I    Oversampling factor
C     ZEROPD       I    Zero padding factor
C     TAPER        C*8  Tapering function ('HANNING' or 'UNIFORM')
C     DELTAT       R    Time interval in table (days) default 2 minutes
C     MCNUMV(21)   I    Element count in each column.
C                        before the creation of a new CL table.
C   Output:
C     IMCRNO       I   Next scan number, start of the file if 'READ',
C                       the last+1 if WRITE
C     MCKOLS(21)   I   The column pointer array in order defined by
C                        the parameters in PCLTAB.INC
C     MCNUMV(21)   I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBSCOD*8, RDATE*8, TAPER*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IMCRNO,
     *   MCKOLS(*), MCNUMV(*), NSTOKE, STOKE1, NUMIF, NCHAN, NUMPOL,
     *   FFTSIZ, OVRSMP, ZEROPD, IERR
      DOUBLE PRECISION RFREQ
      REAL      CHANBW, REFPIX, DELTAT
C
      INTEGER   PKEY, PCOL
      PARAMETER (PKEY=16)
      PARAMETER (PCOL=21)
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE1(11)*24, TITLE2(10)*24, TITLE(PCOL)*24,
     *   UNITS(PCOL)*8, KEYW(PKEY)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, IPT, DTYP(PCOL),
     *   NDATA, KLOCS(PKEY), KEYVAL(2*PKEY), IPOINT, JERR, J, I,
     *   KEYTYP(PKEY), REVNO, MSGSAV, NC, ITRIM
      LOGICAL   DOREAD, NEWFIL
      REAL      KEYR(27)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TITLE(1), TITLE1), (TITLE(12), TITLE2)
      EQUIVALENCE (KEYVAL, KEYR)
      DATA NTT /56/
      DATA TTITLE /'MODEL_COMPS '/
C                                       Values >10 are scalar
      DATA DTYP /11, 14, 14, 14, 14, 11, 11, 11, 11, 11,
     *           11, 02, 02, 12, 12, 11, 11, 02, 02, 12,
     *           12/
C                                       NOTE: Change PCLTAB.INC when
C                                       changing the table columns.
      DATA TITLE1 /'TIME                    ',
     *   'SOURCE_ID               ', 'ANTENNA_NO              ',
     *   'ARRAY                   ', 'FREQID                  ',
     *   'ATMOS                   ', 'DATMOS                  ',
     *   'GDELAY                  ', 'GRATE                   ',
     *   'CLOCK_1                 ', 'DCLOCK_1                '/
      DATA TITLE2 /
     *   'LO_OFFSET_1             ', 'DLO_OFFSET_1            ',
     *   'DISP_1                  ', 'DDISP_1                 ',
     *   'CLOCK_2                 ', 'DCLOCK_2                ',
     *   'LO_OFFSET_2             ', 'DLO_OFFSET_2            ',
     *   'DISP_2                  ', 'DDISP_2                 '/
      DATA KEYW /'OBSCODE ', 'RDATE   ', 'NO_STKD ', 'STK_1   ',
     *           'NO_BAND ', 'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ',
     *           'REF_PIXL', 'NO_POL  ', 'FFT_SIZE', 'OVERSAMP',
     *           'ZERO_PAD', 'TAPER_FN', 'DELTAT', 'TABREV  '/
      DATA UNITS /'DAYS    ', '        ', '        ', '        ',
     *            '        ', 'SECONDS ', 'SEC/SEC ', 'SECONDS ',
     *            'SEC/SEC ', 'SECONDS ', 'SEC/SEC ', 'HZ      ',
     *            'HZ/SEC  ', 'SEC/M**2', 'S/S/M**2', 'SECONDS ',
     *            'SEC/SEC ', 'HZ      ', 'HZ/SEC  ', 'SEC/M**2',
     *            'S/S/M**2'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 9 + NUMPOL * 6
      IF (DOREAD) NCOL = 0
      NKEY = PKEY
      NDATA = PCOL
      CALL FILL (NDATA, 0, MCKOLS)
      CALL FILL (NDATA, 0, MCNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 1,NDATA
            IF (DTYP(J).LT.10) DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
 20   CALL TABINI (OPCODE, 'MC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'MCINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IMCRNO = BUFFER(5) + 1
      IF (DOREAD) IMCRNO = 1
      NKEY = PKEY
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'MCINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'MCINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       OBSCODE
         IPT = 1
         KLOCS(1) = IPT
         KEYTYP(1) = 3
         CALL CHR2H (8, OBSCOD, 1, KEYVAL(1))
         IPT = IPT + 2
C                                       RDATE
         KLOCS(2) = IPT
         KEYTYP(2) = 3
         CALL CHR2H (8, RDATE, 1, KEYVAL(IPT))
         IPT = IPT + 2
C                                       NO_STKD
         KLOCS(3) = IPT
         KEYTYP(3) = 4
         KEYVAL(IPT) = NSTOKE
         IPT = IPT + 1
C                                       STK_1
         KLOCS(4) = IPT
         KEYTYP(4) = 4
         KEYVAL(IPT) = STOKE1
         IPT = IPT + 1
C                                       NO_BAND
         KLOCS(5) = IPT
         KEYTYP(5) = 4
         KEYVAL(IPT) = NUMIF
         IPT = IPT + 1
C                                       NO_CHAN
         KLOCS(6) = IPT
         KEYTYP(6) = 4
         KEYVAL(IPT) = NCHAN
         IPT = IPT + 1
C                                       REF_FREQ
         KLOCS(7) = IPT
         KEYTYP(7) = 1
         CALL RCOPY (NWDPDP, RFREQ, KEYR(IPT))
         IPT = IPT + NWDPDP
C                                       CHAN_BW
         KLOCS(8) = IPT
         KEYTYP(8) = 1
         KEYVAD    = CHANBW
         CALL RCOPY (NWDPDP, KEYVAD, KEYR(IPT))
         IPT = IPT + NWDPDP
C                                       REF_PIXL
         KLOCS(9) = IPT
         KEYTYP(9) = 1
         KEYVAD    = REFPIX
         CALL RCOPY (NWDPDP, KEYVAD, KEYR(IPT))
         IPT = IPT + NWDPDP
C                                       NO_POL
         KLOCS(10) = IPT
         KEYTYP(10) = 4
         KEYVAL(IPT) = NUMPOL
         IPT = IPT + 1
C                                       FFT_SIZE
         KLOCS(11) = IPT
         KEYTYP(11) = 4
         KEYVAL(IPT) = FFTSIZ
         IPT = IPT + 1
C                                       OVERSAMP
         KLOCS(12) = IPT
         KEYTYP(12) = 4
         KEYVAL(IPT) = OVRSMP
         IPT = IPT + 1
C                                       ZERO_PAD
         KLOCS(13) = IPT
         KEYTYP(13) = 4
         KEYVAL(IPT) = ZEROPD
         IPT = IPT + 1
C                                       TAPER_FN
         KLOCS(14) = IPT
         KEYTYP(14) = 3
         CALL CHR2H (8, TAPER, 1, KEYVAL(IPT))
         IPT = IPT + 2
C                                       Time interval
         KLOCS(15) = IPT
         KEYTYP(15) = 1
         KEYVAD    = DELTAT
         IF (DELTAT.LE.0.0) KEYVAD = 2.0D0 / (24.0D0 * 60.0D0)
         CALL RCOPY (NWDPDP, KEYVAD, KEYR(IPT))
         IPT = IPT + NWDPDP
C                                       Revision number
         REVNO = 1
         KLOCS(16) = IPT
         KEYTYP(16) = 4
         KEYVAL(IPT) = REVNO
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'MCINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'MCINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       OBSCODE
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), OBSCOD)
C                                       RDATE
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), RDATE)
C                                       NO_STKD
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NSTOKE = KEYVAL(IPOINT)
C                                       STK_1
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) STOKE1 = KEYVAL(IPOINT)
C                                       NO_BAND
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       NO_CHAN
         IPOINT = KLOCS(6)
         IF (IPOINT.GT.0) NCHAN = KEYVAL(IPOINT)
C                                       REF_FREQ
         IPOINT = KLOCS(7)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(7).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
               RFREQ = KEYVAD
            ELSE
               RFREQ = KEYR(IPOINT)
               END IF
            END IF
C                                       CHAN_BW
         IPOINT = KLOCS(8)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(8).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
               CHANBW = KEYVAD
            ELSE
               CHANBW = KEYR(IPOINT)
               END IF
            END IF
C                                       REF_PIXL
         IPOINT = KLOCS(9)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(9).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
               REFPIX = KEYVAD
            ELSE
               REFPIX = KEYR(IPOINT)
               END IF
            END IF
C                                       NO_POL
         IPOINT = KLOCS(10)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       FFT_SIZE
         IPOINT = KLOCS(11)
         IF (IPOINT.GT.0) FFTSIZ = KEYVAL(IPOINT)
C                                       OVERSAMP
         IPOINT = KLOCS(12)
         IF (IPOINT.GT.0) OVRSMP = KEYVAL(IPOINT)
C                                       ZERO_PAD
         IPOINT = KLOCS(13)
         IF (IPOINT.GT.0) ZEROPD = KEYVAL(IPOINT)
C                                       ZERO_PAD
         IPOINT = KLOCS(14)
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), TAPER)
C                                       time interval
         IPOINT = KLOCS(15)
         IF (IPOINT.GT.0) THEN
            IF (KEYTYP(15).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYR(IPOINT), KEYVAD)
               DELTAT = KEYVAD
            ELSE
               DELTAT = KEYR(IPOINT)
               END IF
            END IF
C                                       Revision level
         IPOINT = KLOCS(16)
         REVNO = -1
         IF (IPOINT.GT.0) REVNO = KEYVAL(IPOINT)
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'MCINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'MC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'MCINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, MCKOLS, JERR)
C                                       check and set
      NCOL = 9 + NUMPOL * 6
      DO 150 I = 1,NDATA
         IPOINT = MCKOLS(I)
         IF (IPOINT.GT.0) THEN
            MCKOLS(I) = DATP(IPOINT,1)
            MCNUMV(I) = DATP(IPOINT,2) / 10
            IF (MCNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF (I.LE.NCOL) CALL MSGWRT (6)
               END IF
         ELSE
            MCKOLS(I) = -1
            MCNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.LE.NCOL) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('MCINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('MCINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('MCINI: ERROR INITIALIZING MC TABLE FOR ',A4)
      END
