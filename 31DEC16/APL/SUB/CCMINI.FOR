      SUBROUTINE CCMINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICCRNO, CCKOLS, CCNUMV, NUMCOL, IERR)
C-----------------------------------------------------------------------
C! Create/open/init I/O to CC model table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2006, 2011
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
C   Creates and initializes component model (CC) extension tables.
C   Inputs:
C      OPCODE  C*4      Operation code:
C                       'WRIT' = create/init for write or read
C                       'READ' = open for read only
C      BUFFER  I(512)   I/O buffer and related storage, also defines
C                       file if open.
C      DISK    I        Disk to use.
C      CNO     I        Catalog slot number
C      VER     I        CC file version
C      CATBLK  I(256)   Catalog header block.
C      LUN     I        Logical unit number to use
C   Input/output
C      NUMCOL  I        Number of columns, 3 => point components only
C   Output:
C      ICCRNO  I        Next scan number, start of the file if 'READ',
C                       the last+1 if WRITE
C      CCKOLS  I(MAXCCC) The column pointer array in order, Flux, X, Y
C                        (and if NUMCOL>3) Major, Minor, PA, type.
C      CCNUMV  I(MAXCCC) Element count in each column.
C      IERR    I        Return error code, 0=>OK, else TABINI or TABIO
C                       error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICCRNO,
     *   CCKOLS(MAXCCC), CCNUMV(MAXCCC), NUMCOL, IERR
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXCCC)*24, UNITS(MAXCCC)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXCCC), NDATA,
     *   I, JERR, NC, ITRIM, IPOINT
      LOGICAL   T, F, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA NTT /56/
      DATA TTITLE /'AIPS CC CLEAN COMPONENTS TABLE'/
      DATA NDATA /MAXCCC/
      DATA DTYP /7*12/
      DATA TITLE /'FLUX', 'DELTAX', 'DELTAY', 'MAJOR AX', 'MINOR AX',
     *   'POSANGLE', 'TYPE OBJ'/
      DATA UNITS /'JY', 5*'DEGREES ', 'CODE'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NCOL = MAX (MIN (NUMCOL, MAXCCC), 3)
      IF (DOREAD) NCOL = 0
      NKEY = 0
      NEWFIL = F
C                                       Fill in types
      CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Create/open file
      CALL TABINI (OPCODE, 'CC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CCMINI', IERR)
         GO TO 990
         END IF
      NUMCOL = NCOL
      NKEY = 0
C                                       See if file exists.
      IF (IERR.LT.0) THEN
         NEWFIL = T
C                                       File created, initialize
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CCMINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CCMINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
         END IF
C                                       Get number of components
      ICCRNO = BUFFER(5)
      ICCRNO = ICCRNO + 1
      IF (DOREAD) ICCRNO = 1
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, BUFFER, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CCMINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CC', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CCMINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXCCC, TITLE, 24, T, BUFFER, CCKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = CCKOLS(I)
         IF (IPOINT.GT.0) THEN
            CCKOLS(I) = DATP(IPOINT,1)
            CCNUMV(I) = DATP(IPOINT,2) / 10
            IF (CCNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF ((NCOL.NE.3) .OR. (I.LE.3)) CALL MSGWRT (6)
               END IF
         ELSE
            CCKOLS(I) = -1
            CCNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF ((NCOL.NE.3) .OR. (I.LE.3)) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CCMINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CCMINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CCMINI: ERROR INITIALIZING CLEAN COMPONENTS TABLE FOR ',
     *   A4)
      END
