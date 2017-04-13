      SUBROUTINE CSINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICSRNO, CSKOLS, CSNUMV, NUMBEM, NUMPOL, NUMIF, IERR)
C-----------------------------------------------------------------------
C! Create/Open/Init Single dish calibration (CS) table
C# EXT-util Calibration Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006
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
C   Creates and initializes single dish calibration (CS) tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        CS file version
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C   Input (create) / output (pre-existing)
C      NUMBEM   I        Number of beams
C      NUMPOL   I        Number of IFs per group
C      NUMIF    I        Number of IF groups
C   Output:
C      ICSRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      CSKOLS   I(13)    The column pointer array in order, TIME,
C                        RA, DEC, BEAM NO., SUBARRAY,
C                        FACTOR 1, OFFSET 1, RAOFF 1, DECOFF 1,
C                        Following used if 2 polarizations per IF
C                        FACTOR 2, OFFSET 2, RAOFF 2, DECOFF 2.
C      CSNUMV   I(13)    Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICSRNO,
     *   CSKOLS(13), CSNUMV(13), NUMBEM, NUMPOL, NUMIF, IERR
c
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(13)*24, UNITS(13)*8, KEYW(3)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(13), NDATA, I,
     *   KLOCS(3), KEYVAL(10), KEYTYP(3), IPOINT, J, JERR, ITRIM, NC
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS SINGLE DISH CALIBRATION TABLE            '/
      DATA NDATA /13/
      DATA DTYP /12,12,12,14,14,8*2/
      DATA TITLE /'TIME                    ',
     *   'RA                      ', 'DEC                     ',
     *   'BEAM NO.                ', 'SUBARRAY                ',
     *   'FACTOR 1                ', 'OFFSET 1                ',
     *   'RAOFF 1                 ', 'DECOFF 1                ',
     *   'FACTOR 2                ', 'OFFSET 2                ',
     *   'RAOFF 2                 ', 'DECOFF 2                '/
      DATA KEYW /'NO_BEAM ', 'NO_POL  ', 'NO_IF   '/
      DATA UNITS /'DAYS    ',
     *   'DEGREE  ', 'DEGREE  ', '        ','        ',
     *   '        ','        ', 'DEGREE  ', 'DEGREE  ',
     *   '        ','        ', 'DEGREE  ', 'DEGREE  '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = 5 + NUMPOL * 4
      NKEY = 3
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 10 J = 6,13
            DATP(J,2) = DTYP(J) + 10 * NUMIF
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CS', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CSINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of scans
      ICSRNO = BUFFER(5) + 1
      IF (DOREAD) ICSRNO = 1
      NKEY = 3
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CSINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CSINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. beams
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMBEM
C                                       No. IFs per group.
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IF groups.
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       Only write if just created.
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CSINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CSINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. beams.
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMBEM = KEYVAL(IPOINT)
C                                       No. IFs per group.
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF groups.
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
         END IF
      IERR = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CSINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CS', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CSINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, CSKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = CSKOLS(I)
         IF (IPOINT.GT.0) THEN
            CSKOLS(I) = DATP(IPOINT,1)
            CSNUMV(I) = DATP(IPOINT,2) / 10
            IF (CSNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CSKOLS(I) = -1
            CSNUMV(I) = 0
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
 1100 FORMAT ('CSINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CSINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CSINI: ERROR INITIALIZING CALIBRATION TABLE FOR ',A4)
      END
