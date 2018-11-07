      SUBROUTINE WXINI (OPCODE, WXBUFF, DISK, CNO, VER, CATBLK, LUNWX,
     *   IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IERR)
C-----------------------------------------------------------------------
C! creates and intializes a weather data (WX) table.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2001-2003, 2006, 2010, 2015-2016
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
C   Creates and initializes weather data tables.
C   Inputs:
C      OPCODE         C*4  Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      WXBUFF(512)    I    I/O buffer and related storage, also defines
C                          file if open.
C      DISK           I    Disk to use.
C      CNO            I    Catalog slot number
C      VER            I    WX file version
C      CATBLK(256)    I    Catalog header block.
C      LUNWX          I    Logical unit number to use
C   Input (create) / output (pre-existing) (file keywords):
C      OBSCOD         C*8  Observing code.
C      OBSDAT         C*8  Observing date.
C      TABVER         I    Table revision number.
C      WXNUMV(MAXWXC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output:
C      IWXRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      WXKOLS(MAXWXC) I    The column pointer array in order:
C                          TIME, DTIM, ANT, TEMP, PRESS, DEWPT,
C                          WVEL, WDIR, H2OCOL, IONCOL
C   Output:
C      IERR           I    Return error code, 0=>OK, else TABINI or
C                          TABIO error.
C----------------------------------------------------------------------
      INCLUDE 'INCS:DWXV.INC'
      CHARACTER OPCODE*4, OBSCOD*8, OBSDAT*8
      INTEGER   WXBUFF(512), DISK, CNO, VER, CATBLK(256), LUNWX, IWXRNO,
     *   WXKOLS(MAXWXC), WXNUMV(MAXWXC), TABVER, IERR
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL T, DOREAD, NEWFIL
      CHARACTER TTITLE*56, TITLE(MAXWXC)*24, TITLE2(MAXWXC)*24,
     *   UNITS(MAXWXC)*8, KEYWRD(NKEYWX)*8, TITLEC(MAXWXC)*24
      INTEGER   I, JERR, ITEMP(6), NTTWX, NKEY, NREC, NCOL, IPOINT,
     *   NC, ITRIM, NDATA, DATP(128,2), KLOCS(NKEYWX), KEYTYP(NKEYWX),
     *   KEYI(NKYWWX), DTYP(MAXWXC), WXKOL2(MAXWXC), WXKOLC(MAXWXC)
      REAL      KEYR(NKYWWX)
      DOUBLE PRECISION KEYD(NKYWW2)
      HOLLERITH KEYH(NKYWWX), HOLTMP(6)
      EQUIVALENCE (KEYI, KEYR, KEYD, KEYH)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA TTITLE / 'WEATHER DATA TABLE' /
      DATA TITLE /'TIME', 'TIME_INTERVAL', 'ANTENNA_NO', 'SUBARRAY',
     *   'TEMPERATURE', 'PRESSURE', 'DEWPOINT', 'WIND_VELOCITY',
     *   'WIND_DIRECTION', 'WIND_GUST', 'PRECIPITATION', 'WVR_H2O',
     *   'IONOS_ELECTRON' /
C                                       casa
      DATA TITLEC /'TIME', 'TIME_INTERVAL', 'ANTENNA NO.', 'SUBARRAY',
     *   'TEMPERATURE', 'PRESSURE', 'DEWPOINT', 'WIND_VELOCITY',
     *   'WIND_DIRECTION', 'WIND_GUST', 'PRECIPITATION', 'WVR_H2O',
     *   'IONOS_ELECTRON' /
      DATA TITLE2 / 'TIME', 'TIME INTERVAL', 'ANTENNA NUMBER',
     *   'SUBARRAY', 'TEMPERATURE', 'PRESSURE', 'DEWPOINT',
     *   'WIND VELOCITY', 'WIND DIRECTION', 'WIND GUST',
     *   'PRECIPITATION', 'H2O COLUMN', 'ELECTRON COLUMN' /
      DATA UNITS / 2*'DAYS', 2*' ', 'CENTIGRA', 'MILLIBAR',
     *   'CENTIGRA', 'M/SEC', 'DEGREES', 'M/SEC', 'CM', 2*'/M/M' /
      DATA KEYWRD / 'OBSCODE', 'RDATE', 'TABREV' /
      DATA NTTWX / 56 /
      DATA T / .TRUE. /
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 1000
      NCOL = MAXWXC
      IF (DOREAD) NCOL = 0
      NKEY = NKEYWX
      NDATA = MAXWXC
      CALL FILL (NDATA, 0, WXKOLS)
      CALL FILL (NDATA, 0, WXNUMV)
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(WXDTIM) = TABDBL + 10
         DTYP(WXRINT) = TABFLT + 10
         DTYP(WXIANT) = TABINT + 10
         DTYP(WXSUBA) = TABINT + 10
         DTYP(WXRTMP) = TABFLT + 10
         DTYP(WXRPRS) = TABFLT + 10
         DTYP(WXRDWP) = TABFLT + 10
         DTYP(WXRVEL) = TABFLT + 10
         DTYP(WXRDIR) = TABFLT + 10
         DTYP(WXRGUS) = TABFLT + 10
         DTYP(WXRPRE) = TABFLT + 10
         DTYP(WXRH2O) = TABFLT + 10
         DTYP(WXRION) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'WX', DISK, CNO, VER, CATBLK, LUNWX, NKEY,
     *   NREC, NCOL, DATP, WXBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WXINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IWXRNO = WXBUFF(5) + 1
      IF (DOREAD) IWXRNO = 1
      NKEY = NKEYWX
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, WXBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WXINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, WXBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'WXINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTTWX, TTITLE, 1, WXBUFF(101))
C                                       Set keyword values
C                                       Observing code
         KLOCS(1) = 1
         KEYTYP(1) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KEYH(KLOCS(1)))
C                                       Observing date
         KLOCS(2) = 3
         KEYTYP(2) = TABHOL
         CALL CHR2H (8, OBSDAT, 1, KEYH(KLOCS(2)))
C                                       Table revision number
         KLOCS(3) = 5
         KEYTYP(3) = TABINT
         KEYI(KLOCS(3)) = IWXREV
C                                       Write to the WX table
         CALL TABKEY ('WRIT', KEYWRD, NKEYWX, WXBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'WXINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYWRD, NKEYWX, WXBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'WXINI', IERR)
            GO TO 990
            END IF
C                                       Observing code
         CALL H2CHR (8, 1, KEYH(KLOCS(1)), OBSCOD)
C                                       Observing date
         CALL H2CHR (8, 1, KEYH(KLOCS(2)), OBSDAT)
C                                       Table revision number
         TABVER = KEYI(KLOCS(3))
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYI, WXBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'WXINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'WX', DISK, CNO, VER, CATBLK, LUNWX, NKEY,
     *   NREC, NCOL, DATP, WXBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'WXINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXWXC, TITLE, 24, T, WXBUFF, WXKOLS, JERR)
      CALL FNDCOL (MAXWXC, TITLE2, 24, T, WXBUFF, WXKOL2, JERR)
      CALL FNDCOL (MAXWXC, TITLEC, 24, T, WXBUFF, WXKOLC, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXWXC
         IPOINT = WXKOLS(I)
         IF (IPOINT.EQ.0) IPOINT = WXKOL2(I)
         IF (IPOINT.EQ.0) IPOINT = WXKOLC(I)
         IF (IPOINT.GT.0) THEN
            WXKOLS(I) = DATP(IPOINT,1)
            WXNUMV(I) = DATP(IPOINT,2) / 10
            IF (WXNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF (I.LE.9) CALL MSGWRT (6)
               END IF
         ELSE
            WXKOLS(I) = -1
            WXNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.LE.9) CALL MSGWRT (6)
            END IF
 150     CONTINUE
C                                       check format of time (CASA)
      IPOINT = WXKOLS(1)
      I = MOD (DATP(IPOINT,2), 10)
      IF (I.EQ.2) WXNUMV(1) = -WXNUMV(1)
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('WXINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('WXINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('WXINI: ERROR INITIALIZING WEATHER INFO TABLE FOR ', A4)
      END
