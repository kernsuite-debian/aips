      SUBROUTINE OTTINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IERR)
C-----------------------------------------------------------------------
C! Create/open/init over-the-top (OT) table
C# EXT-util Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2011
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
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   OT file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Output:
C     IERR           I   Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   Output TO DOTTV.INC  /OTTABV/:
C     IOTRNO         I   Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C     OTKOLS(MAXOTC)  I   The column pointer array in order, TIME,
C                         TIME INTERVAL, SOURCE ID, ANTENNA-NUMBER,
C                         OVER-THE-TOP
C     OTNUMV(MAXOTC) I   Element count in each column.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IERR
c
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DOTTV.INC'
      CHARACTER TITLE(MAXOTC)*24, UNITS(MAXOTC)*8, TTITLE*41
      HOLLERITH HOLTMP(6)
      INTEGER   NTT, I, NKEY, NC, JERR, ITRIM
      LOGICAL   DOREAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /41/
      DATA TTITLE /'AIPS UV DATA FILE SCAN OVER THE TOP TABLE'/
      DATA TITLE
     *  /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ',
     *   'ANTENNA                 ',
     *   'OVER TOP                '/
      DATA UNITS /'DAYS    ', 'DAYS    ', 3*'        '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NCOL = MAXOTC
      NREC = 1000
      CALL FILL (NCOL, 0, OTKOLS)
      CALL FILL (NCOL, 0, OTNUMV)
C                                       Types, lengths of each column.
C                                       OT pointers defined in DOTTV.INC
C                                       Only used when creating table.
      IF (.NOT.DOREAD) THEN
         DTYP(OTRTIM) = TABDBL + 10
         DTYP(OTRTMI) = TABFLT + 10
         DTYP(OTISID) = TABINT + 10
         DTYP(OTIANT) = TABINT + 10
         DTYP(OTLOTT) = TABLOG + 10
C                                       Note: DATP(*,1) contains address
C                                       pointers. Used on output only;
C                                       no need to fill it here.
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'OT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OTTINI', IERR)
         GO TO 990
         END IF
C                                       Get number of rows
      IOTRNO = BUFFER(5) + 1
      IF (DOREAD) IOTRNO = 1
      TDUBL = DATP(OTRTIM,2).EQ.TABDBL+10
C                                       File created, initialize
      IF (IERR.LT.0) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OTTINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'OTTINI', IERR)
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
         CALL TABERR ('CLOS', 'TABIO', 'OTTINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'OT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'OTTINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXOTC, TITLE, 24, .TRUE., BUFFER, OTKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,MAXOTC
         IPOINT = OTKOLS(I)
         IF (IPOINT.GT.0) THEN
            OTKOLS(I) = DATP(IPOINT,1)
            OTNUMV(I) = DATP(IPOINT,2) / 10
            IF (OTNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            OTKOLS(I) = -1
            OTNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('OTTINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('OTTINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('OTTINI: ERROR INITIALIZING OVER-THE-TOP TABLE FOR ',A4)
      END

