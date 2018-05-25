      SUBROUTINE FCINI (OPCODE, LUN, VOL, CNO, VER, CATBLK, FCNUM,
     *   LASTR, BUFFER, FCKOLS, FCNUMV, IERR)
C-----------------------------------------------------------------------
C! Opens/creates FC (Flag Command) tables used by OOP EDIT class
C# EXT-util UV Calibration OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1998, 2006
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
C   creates and/or opens for writing (and reading) a specified FC table
C   for Flag Commands from OOP EDIT class
C   Inputs:
C      OPCODE  C*4       READ or WRIT (create only if none previously)
C      LUN     I         Logical unit number to use
C      VOL     I         Disk number
C      CNO     I         Catalog number
C   In/out:
C      VER     I         Input: desired version number 0 -> highest
C                           existing or 1 if none previously
C                        Output: that used
C      CATBLK  I(256)    File catalog header block
C   Output:
C      FCNUM   I         Highest current flag command number
C      LASTR   I         Highest current record written
C      BUFFER  I(512)    Required for later calls to TABIO
C      FCKOLS  I(11)     Column pointers
C      FCNUMV  I(11)     Count by column
C      IERR    I         Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      INTEGER   NCOLS
      PARAMETER (NCOLS=15)
C
      CHARACTER OPCODE*4
      INTEGER   LUN, VOL, CNO, VER, CATBLK(256), FCNUM, LASTR,
     *   BUFFER(512), FCKOLS(NCOLS), FCNUMV(NCOLS), IERR
C
      INTEGER   IRNO, NKEY, NREC, ITITLE(8), DATP(128,2), CCODE(NCOLS),
     *   NCOL, RECORD(29), NUMBP, I, ITRIM, NC, IPOINT, JERR
      HOLLERITH HTITLE(8)
      CHARACTER TTITLE*32, CTITLE(NCOLS)*8, UNITS(NCOLS)*8, TITLE*24
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITITLE, HTITLE)
      DATA TTITLE /'AIPS UV EDITOR FLAG COMMAND TABL'/
      DATA CTITLE /'FLAGNUMB', 'FLAGOPER', 'FLAGTIME', 'FLAGANT',
     *   'FLAGSOUR', 'FLAGCHAN', 'FLAGIF  ', 'FLAGSUBA', 'FLAGFQID',
     *   'FLAGSTOK', 'DATATYPE', 'CLIPRANG', 'TIMERANG', 'ITIME',
     *   'REASON'/
      DATA UNITS /2*' ', 'DAYS', 6*' ', 'STK CODE', 2*' ', 'DAYS',
     *   2*' '/
      DATA CCODE /14, 83, 22, 24, 14, 24, 24, 14, 14, 43, 83, 22, 22,
     *   24, 243/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = NCOLS
      NKEY = 1
      NREC = 500
      IF (OPCODE.NE.'READ') THEN
         CALL FILL (256, 0, DATP)
         CALL COPY (NCOL, CCODE, DATP(1,2))
         END IF
C                                       Version number
      IF (VER.LE.0) CALL FNDEXT ('FC', CATBLK, VER)
C                                       create/open
      CALL TABINI (OPCODE, 'FC', VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, VER
         CALL MSGWRT (7)
C                                       pre-existing file
      ELSE IF (IERR.EQ.0) THEN
         NUMBP = DATP(1,1)
         IF (BUFFER(5).GT.0) THEN
            CALL TABIO ('READ', 0, BUFFER(5), RECORD, BUFFER, IERR)
            FCNUM = RECORD(NUMBP)
            LASTR = BUFFER(5)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, VER, BUFFER(5)
               CALL MSGWRT (7)
               END IF
         ELSE
            FCNUM = 0
            LASTR = 0
            END IF
C                                       New file created
      ELSE
         FCNUM = 0
         LASTR = 0
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FCINI', IERR)
               GO TO 990
               END IF
 20         CONTINUE
C                                       write units
         DO 30 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'FCINI', IERR)
               GO TO 990
               END IF
 30         CONTINUE
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUFFER(101))
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 3, IRNO, HTITLE, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'FCINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'FC', VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'FCINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NCOLS, CTITLE, 8, T, BUFFER, FCKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NCOLS
         IPOINT = FCKOLS(I)
         IF (IPOINT.GT.0) THEN
            FCKOLS(I) = DATP(IPOINT,1)
            FCNUMV(I) = DATP(IPOINT,2) / 10
            IF (FCNUMV(I).LE.0) THEN
               NC = ITRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            FCKOLS(I) = -1
            FCNUMV(I) = 0
            NC = ITRIM (CTITLE(I))
            WRITE (MSGTXT,1101) CTITLE(I)(:NC)
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
 1000 FORMAT ('ERROR',I5,' OPENING Flag-Command TABLE VERSION',I4)
 1010 FORMAT ('ERROR',I5,' READING Flag-Command TABLE VERSION',I4,
     *   ' RECORD',I8)
 1100 FORMAT ('FCINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('FCINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('FCINI: ERROR INITIALIZING FLAG CONTROL TABLE FOR ',A4)
      END
