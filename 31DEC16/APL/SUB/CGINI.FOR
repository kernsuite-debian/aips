      SUBROUTINE CGINI (OPCODE, CGBUFF, DISK, CNO, VER, CATBLK, LUNCG,
     *   ICGRNO, CGKOLS, CGNUMV, TABVER, IERR)
C-----------------------------------------------------------------------
C! creates and intializes a Clean beam (CG) table.
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2010
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
C   Creates and initializes Clean beam tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                          'WRIT' = create/init for write or read
C                          'READ' = open for read only
C      CGBUFF   I(512)   I/O buffer and related storage, also defines
C                         file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VER      I        CG file version
C      CATBLK   I(256)   Catalog header block.
C      LUNCG    I        Logical unit number to use
C   Output:
C      ICGRNO   I        Next row number, start of the file if READ,
C                          the last+1 if WRITE
C      CGNUMV   I(4)     All 1
C      CGKOLS   I(4)     The column pointer array in order:
C                          Frequency, Bmaj, Bmin, Bpa
C   Output:
C      IERR     I        Return error code, 0=>OK, else TABINI or
C                        TABIO error.
C----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   CGBUFF(512), DISK, CNO, VER, CATBLK(256), LUNCG, ICGRNO,
     *   CGKOLS(4), CGNUMV(4), TABVER, IERR
C
      INTEGER   NKEYCG, NCOLCG
      PARAMETER (NKEYCG=1)
      PARAMETER (NCOLCG=4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PTAB.INC'
      LOGICAL   T, DOREAD, NEWFIL
      CHARACTER TTITLE*56, TITLE(NCOLCG)*24, UNITS(NCOLCG)*8,
     *   KEYWRD(NKEYCG)*8
      INTEGER   I, JERR, ITEMP(6), NTTCG, NKEY, NREC, NCOL, IPOINT,
     *   NC, ITRIM, NDATA, DATP(128,2), KLOCS(NKEYCG), KEYTYP(NKEYCG),
     *   KEYI(NKEYCG), DTYP(NCOLCG), ICGREV
      HOLLERITH HOLTMP(6)
      EQUIVALENCE (HOLTMP, ITEMP)
      DATA TTITLE / 'CLEAN BEAM TABLE' /
      DATA TITLE /'FREQUENCY', 'BMAJ', 'BMIN', 'BPA'/
      DATA UNITS / 'HZ', 3*'DEGREES'/
      DATA KEYWRD /'TABREV' /
      DATA NTTCG / 56 /
      DATA T / .TRUE. /
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
      ICGREV = 1
C                                       Set up needed variables
      NREC = 1000
      NCOL = NCOLCG
      IF (DOREAD) NCOL = 0
      NKEY = NKEYCG
      NDATA = NCOLCG
      CALL FILL (NDATA, 0, CGKOLS)
      CALL FILL (NDATA, 0, CGNUMV)
C                                       Fill in types, lengths
C                                       See Going AIPS Vol 2 p13-3.
      IF (.NOT.DOREAD) THEN
         DTYP(1) = TABDBL + 10
         DTYP(2) = TABFLT + 10
         DTYP(3) = TABFLT + 10
         DTYP(4) = TABFLT + 10
         CALL COPY (NCOL, DTYP, DATP(1,2))
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CG', DISK, CNO, VER, CATBLK, LUNCG, NKEY,
     *   NREC, NCOL, DATP, CGBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CGINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of records
      ICGRNO = CGBUFF(5) + 1
      IF (DOREAD) ICGRNO = 1
      NKEY = NKEYCG
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, CGBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CGINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, CGBUFF, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CGINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTTCG, TTITLE, 1, CGBUFF(101))
C                                       Set keyword values
C                                       Table revision number
         KLOCS(1) = 5
         KEYTYP(1) = TABINT
         KEYI(KLOCS(1)) = ICGREV
C                                       Write to the CG table
         CALL TABKEY ('WRIT', KEYWRD, NKEYCG, CGBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CGINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         CALL TABKEY ('READ', KEYWRD, NKEYCG, CGBUFF, KLOCS, KEYI,
     *      KEYTYP, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'CGINI', IERR)
            GO TO 990
            END IF
C                                       Table revision number
         TABVER = KEYI(KLOCS(1))
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYI, CGBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CGINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CG', DISK, CNO, VER, CATBLK, LUNCG, NKEY,
     *   NREC, NCOL, DATP, CGBUFF, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CGINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (4, TITLE, 24, T, CGBUFF, CGKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,4
         IPOINT = CGKOLS(I)
         IF (IPOINT.GT.0) THEN
            CGKOLS(I) = DATP(IPOINT,1)
            CGNUMV(I) = DATP(IPOINT,2) / 10
            IF (CGNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CGKOLS(I) = -1
            CGNUMV(I) = 0
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
 1100 FORMAT ('CGINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CGINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CGINI: ERROR INITIALIZING WEATHER INFO TABLE FOR ', A4)
      END
