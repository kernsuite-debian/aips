      SUBROUTINE AGINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! creates and intializes a temporary array geometry table
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
C   Creates and initializes array geometry tables.
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
C      VER            I    AG file version
C   Input (create) / output (pre-existing) in common (file keywords):
C      ARRAYC(3)      D    Array center X coord. (meters, earth center)
C      SAFREQ         D    Obs. Reference Frequency for subarray(Hz)
C      RDATE          C*8  Reference date as 'DD/MM/YY'
C      ANAME          C*8  Array name
C      NUMORB         I    Number of orbital parameters
C      FRAME          C*8  Reference frame of corrdinate system
C                          (e.g. 'GEOCENTR')
C      TIMSYS         C*8  Time system, 'IAT' or 'UTC'
C      GSTIA0         D    GST at time(c.f TIMSYS) = 0hr on ref
C                          date (degrees)
C      DEGPDY         D    Earth rotation rate (deg/IAT day)
C      POLARX         R    Polar position X (metres) on ref. date
C      POLARY         R    Polar position Y (metres) on ref. date
C      UT1UTC         R    UT1 - UTC (time seconds)
C      IATUTC         R    IAT - UTC (time seconds)
C      OBSCODE        C*8  Observing code
C      NOSTKD         I    No polzns in the data
C      STK1           I    First Stokes parameter in the data
C      NOBAND         I    The number of bands (IF's) in the data.
C      NOCHAN         I    The number of spectral channels in the data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented, PJD, Feb 1, 1991
C      AGNUMV(MAXAGC) I    Element count in each column. On input only
C                          used if the file is created.
C   Output in common:
C      IAGRNO         I    Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      AGKOLS(MAXAGC) I    The column pointer array in order, ANNAME,
C                          STABXYZ, DERXYZ, ORBPARM, NOSTA, MNTSTA,
C                          STAXOF
C   Output:
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
C                                       AG data statements
      INCLUDE 'INCS:DAGV.INC'
      CHARACTER  KEYW(NKEYAG)*8, TITLE(MAXAGC)*24, UNITS(MAXAGC)*8,
     *   TTITLE*56
      DATA NTT /56/
      DATA TTITLE /'ARRAY GEOMETRY TABLE'/
C                                       Table column titles
      DATA TITLE /'ANNAME  ', 'STABXYZ ', 'DERXYZ  ', 'ORBPARM ',
     *   'NOSTA   ', 'MNTSTA  ', 'STAXOF  ', 'DIAMETER' /
C                                       Units of table columns
      DATA UNITS /' ', 'METERS ', 'M/S     ', 3*' ', 'METERS ',
     *   'METERS ' /
C                                       Header keywords
      DATA KEYW /'ARRAYX  ', 'ARRAYY  ', 'ARRAYZ  ', 'FREQ    ',
     *   'RDATE   ', 'ARRNAM  ', 'NUMORB  ', 'FRAME   ', 'TIMSYS  ',
     *   'GSTIA0', 'DEGPDY', 'POLARX', 'POLARY',
     *   'UT1UTC', 'IATUTC', 'OBSCODE ',
     *   'NO_STKD ', 'STK_1   ', 'NO_BAND ', 'NO_CHAN',
     *   'REF_FREQ', 'CHAN_BW ', 'REF_PIXL', 'TABREV' /
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Set up needed variables
      NREC = 30
      NCOL = MAXAGC
      NKEY = NKEYAG
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DTYP(KANN) = TABHOL + 10*8
         DTYP(KSTA) = TABDBL + 10*3
         DTYP(KDER) = TABFLT + 10*3
         DTYP(KORB) = TABDBL + 10*NUMORB
         DTYP(KNOS) = TABINT + 10
         DTYP(KMNT) = TABINT + 10
         DTYP(KSTX) = TABFLT + 10*3
         DTYP(KDIA) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'AG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'AGINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      IAGRNO = BUFFER(5) + 1
      IF (DOREAD) IAGRNO = 1
      NKEY = NKEYAG
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'AGINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'AGINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         CALL AGIO ('WRIT', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'AGIO', 'AGINI', IERR)
            GO TO 990
            END IF
C                                       Write if just created
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'AGINI', IERR)
            GO TO 990
            END IF
C                                       read keywords to common
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'AGINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
         CALL AGIO ('READ', KEYVAL, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('READ', 'AGIO', 'AGINI', IERR)
            GO TO 990
            END IF
         END IF
      IERR = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'AGINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'AG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'AGINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXAGC, TITLE, 24, T, BUFFER, AGKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXAGC
         IPOINT = AGKOLS(I)
         IF (IPOINT.GT.0) THEN
            AGKOLS(I) = DATP(IPOINT,1)
            AGNUMV(I) = DATP(IPOINT,2) / 10
            IF (AGNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF ((TITLE(I).NE.'ORBPARM') .AND.
     *            (TITLE(I).NE.'DIAMETER')) CALL MSGWRT (6)
               END IF
         ELSE
            AGKOLS(I) = -1
            AGNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF ((TITLE(I).NE.'ORBPARM') .AND.
     *         (TITLE(I).NE.'DIAMETER')) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('AGINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('AGINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('AGINI: ERROR INITIALIZING ARRAY GEOMETRY TABLE FOR ',A4)
      END
