      SUBROUTINE NDXINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
C-----------------------------------------------------------------------
C! Create/open/init index (NX) table
C# EXT-util Calibration UV
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
C   Creates and initializes INDEX extention tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   In/out:
C      VER      I        NX file version
C      CATBLK   I(256)   Catalog header block.
C   Output:
C      INXRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      NXKOLS   I(MAXNXC)   The column pointer array in order, TIME,
C                        TIME INTERVAL, SOURCE ID, SUBARRAY, START VIS,
C                        END VIS, FREQID.
C      NXNUMV   I(MAXNXC)   Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, INXRNO,
     *   NXKOLS(*), NXNUMV(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TITLE(MAXNXC)*24, UNITS(MAXNXC)*8, TTITLE*56
      HOLLERITH HOLTMP(6)
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXNXC), NDATA,
     *   I, IPOINT, NC, ITRIM, JERR
      LOGICAL   DOREAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE SCAN INDEX TABLE '/
      DATA DTYP /12,12,14,14,14,14,14/
      DATA TITLE /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ',
     *   'SUBARRAY                ', 'START VIS               ',
     *   'END VIS                 ',
     *   'FREQ ID                 '/
      DATA UNITS /'DAYS    ', 'DAYS    ', 5*'        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NDATA = MAXNXC
      NREC = 20
      NCOL = NDATA
      NKEY = 0
      CALL FILL (NDATA, 0, NXKOLS)
      CALL FILL (NDATA, 0, NXNUMV)
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DO 10 I = 1,NDATA
            DATP(I,2) = DTYP(I)
            NXNUMV(I) = DTYP(I) / 10
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'NX', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'NDXINI', IERR)
         GO TO 990
         END IF
C                                       Get number of scans
      INXRNO = BUFFER(5) + 1
      IF (DOREAD) INXRNO = 1
C                                       File created, initialize
      IF (IERR.LT.0) THEN
C                                       Col. labels.
         DO 40 I = 1,NDATA
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'NDXINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'NDXINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, BUFFER, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'NDXINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'NX', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'NDXINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXNXC, TITLE, 24, .TRUE., BUFFER, NXKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXNXC
         IPOINT = NXKOLS(I)
         IF (IPOINT.GT.0) THEN
            NXKOLS(I) = DATP(IPOINT,1)
            NXNUMV(I) = DATP(IPOINT,2) / 10
            IF (NXNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            NXKOLS(I) = -1
            NXNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      IERR = 0
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('NDXINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('NDXINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('NDXINI: ERROR INITIALIZING SCAN INDEX TABLE FOR ',A4)
      END
