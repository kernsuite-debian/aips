      SUBROUTINE STINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ISTRNO, STKOLS, STNUMV, IERR)
C-----------------------------------------------------------------------
C! Opens/creates ST ("star") tables
C# EXT-util Plot Analysis
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   creates and/or opens for writing (and reading) a specified ST table
C   for star positions
C   Inputs:
C      OPCODE   C*4      READ or WRIT (create only if none previously)
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      LUN      I        Logical unit number to use
C   In/out:
C      VER      I        Input: desired version number 0 -> highest
C                               existing or 1 if none previously
C                        Output: that used
C      CATBLK   I(*)     File catalog header block
C   Output:
C      BUFFER   I(512)   Required for later calls to TABIO
C      ISTRNO   I        Next record to read (=1) or write (=max+1)
C      STKOLS   I(7)     Column pointers
C      STNUMV   I(7)     Count by column
C      IERR     I        Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      INTEGER   NCOLS, MXSTLB
      PARAMETER (NCOLS=7)
      PARAMETER (MXSTLB=24)
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(*), LUN, ISTRNO,
     *   STKOLS(NCOLS), STNUMV(NCOLS), IERR
C
      INTEGER   IRNO, NKEY, NREC, DATP(128,2), CCODE(NCOLS), NCOL, I, J,
     *   JTRIM, NC, IPOINT, JERR
      HOLLERITH HTITLE(8)
      CHARACTER TTITLE*32, CTITLE(NCOLS)*8, UNITS(NCOLS)*8, TITLE*24,
     *   ATYPE(18)*4, NTYPE(7)*8
      LOGICAL   T, NEWFIL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TTITLE /'AIPS ST star positions table'/
      DATA CTITLE /' ', ' ', 'MAJOR AX', 'MINOR AX', 'POSANG',
     *   'STARTYPE', 'LABEL'/
      DATA UNITS /4*' ', 'DEGREES', 'INDEX', 'STRING'/
      DATA CCODE /11, 11, 12, 12, 12, 12, 3/
      DATA T /.TRUE./
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
      DATA NTYPE /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'DEGREES '/
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = NCOLS
      NKEY = 1
      NREC = 100
      IF (OPCODE.NE.'READ') THEN
         CALL FILL (256, 0, DATP)
         CCODE(7) = MXSTLB * 10 + 3
         CALL COPY (NCOL, CCODE, DATP(1,2))
         END IF
C                                       create/open
      CALL TABINI (OPCODE, 'ST', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SNINI', IERR)
         GO TO 990
         END IF
C                                       okay
      NEWFIL = IERR.LT.0
      ISTRNO = BUFFER(5) + 1
      IF (OPCODE.EQ.'READ') ISTRNO = 1
      CALL H2CHR (8, 1, CATBLK(KHCTP), CTITLE(1))
      CALL H2CHR (8, 1, CATBLK(KHCTP+2), CTITLE(2))
C                                       New file created
      IF (NEWFIL) THEN
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'STINI', IERR)
               GO TO 990
               END IF
 20         CONTINUE
C                                       figure out units
         DO 30 I = 1,2
            DO 25 J = 1,18
               IF (CTITLE(I)(:4).EQ.ATYPE(J)) THEN
                  UNITS(I) = NTYPE(MIN(7,J))
                  UNITS(5-I) = UNITS(I)
                  GO TO 30
                  END IF
 25            CONTINUE
 30         CONTINUE
C                                       write units
         DO 40 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'STINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       table title
         CALL CHR2H (32, TTITLE, 1, BUFFER(101))
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 3, IRNO, HTITLE, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'STINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'ST', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'STINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NCOLS, CTITLE, 8, T, BUFFER, STKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NCOLS
         IPOINT = STKOLS(I)
         IF (IPOINT.GT.0) THEN
            STKOLS(I) = DATP(IPOINT,1)
            STNUMV(I) = DATP(IPOINT,2) / 10
            IF (STNUMV(I).LE.0) THEN
               NC = JTRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            STKOLS(I) = -1
            STNUMV(I) = 0
            NC = JTRIM (CTITLE(I))
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
 1100 FORMAT ('STINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('STINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('STINI: ERROR INITIALIZING STARS TABLE FOR ',A4)
      END
