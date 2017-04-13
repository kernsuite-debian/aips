      SUBROUTINE FLGINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IERR)
C-----------------------------------------------------------------------
C! Create/Open/Init Flag (FG) table.
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2006
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
C   Creates and initializes FLAG extension tables.
C    Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   FG file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C    Output:
C     IFGRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     FGKOLS(MAXFGC)   I   The column pointer array in order, SOURCE,
C                      SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                      PFLAGS, REASON
C     FGNUMV(MAXFGC)    I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IFGRNO,
     *   FGKOLS(*), FGNUMV(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TITLE(MAXFGC)*24, UNITS(MAXFGC)*8, TTITLE*56
      HOLLERITH HOLTMP(6)
      INTEGER   NTT, NKEY, NREC, DATP(128,2), NCOL, DTYP(MAXFGC), NDATA,
     *   I, JERR, NC, ITRIM, IPOINT
      LOGICAL   DOREAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE FLAG TABLE '/
      DATA DTYP /14,14,14,24,22,24,24,47,243/
      DATA TITLE /'SOURCE                  ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'ANTS                    ',
     *   'TIME RANGE              ', 'IFS                     ',
     *   'CHANS                   ',
     *   'PFLAGS                  ', 'REASON                  '/
      DATA UNITS /4*'        ', 'DAYS    ', 4*'        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NDATA = MAXFGC
      NREC = 500
      NCOL = NDATA
      NKEY = 0
C                                       Fill in types
      IF (.NOT.DOREAD) CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Create/open file
      CALL TABINI (OPCODE, 'FG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'FLGINI', IERR)
         GO TO 990
         END IF
C                                       See if file exists.
      IF (IERR.LT.0) THEN
C                                       File created, initialize
         DO 40 I = 1,NDATA
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FLGINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FLGINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
         END IF
C                                       Get number of scans
      IFGRNO = BUFFER(5)
      IFGRNO = IFGRNO + 1
      IF (DOREAD) IFGRNO = 1
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, HOLTMP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FLGINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'FG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'FLGINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXFGC, TITLE, 24, .TRUE., BUFFER, FGKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXFGC
         IPOINT = FGKOLS(I)
         IF (IPOINT.GT.0) THEN
            FGKOLS(I) = DATP(IPOINT,1)
            FGNUMV(I) = DATP(IPOINT,2) / 10
            IF (FGNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            FGKOLS(I) = -1
            FGNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FLGINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('FLGINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('FLGINI: ERROR INITIALIZING FLAG TABLE FOR ',A4)
      END
