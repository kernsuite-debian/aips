      SUBROUTINE SUINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
C-----------------------------------------------------------------------
C! Create/initialize/open source (SU) table
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006, 2012, 2014
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
C   Creates and initializes SOURCE (SU) extension tables or opens them
C   for read.  USE SOUINI --- not this routine --- to insure correct
C   formating.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   SU file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Input (create) / Output (pre-existing) :
C     NUMIF        I   Table keyword, gives the number of IFs
C     VELTYP       C*8 Velocity type,
C     VELDEF       C*8 Velocity defination 'RADIO','OPTICAL',
C     FREQID       I   Table keyword, denotes the FQ ID for which
C                      the SU parms have been modified. On O/P if
C                      FREQID = -999 it is not in the table, if
C                      FREQID = -1 the virgin values still exist, or
C                      the data have no FREQID random parameter.
C   Output:
C     ISURNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     SUKOLS(MAXSUC)I   The column pointer array in order, ID. NO.,
C                      SOURCE, QUAL, CALCODE, IFLUX, QFLUX, UFLUX,
C                      VFLUX, FREQO, BANDWIDTH, RAEPO, DECEPO, EPOCH,
C                      RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
C                      PMRA, PMDEC
C     SUNUMV(MAXSUC)I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, VELTYP*8, VELDEF*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, NUMIF,
     *   FREQID, ISURNO, SUKOLS(*), SUNUMV(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TITLE(MAXSUC)*24, TTITLE*56, KEYW(4)*8, UNITS(MAXSUC)*8
      HOLLERITH HOLTMP(6)
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, I, NTT, DTYP(MAXSUC),
     *   NDATA, KLOCS(4), KEYVAL(10), IT, KEYTYP(4), IPOINT, MSGSAV, NC,
     *   ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE SOURCE TABLE '/
      DATA NDATA /21/
      DATA DTYP /14,163,14,43,2,2,2,2,1,11,11,11,11,11,11,11,11,
     *   1,1,11,11/
      DATA TITLE /'ID. NO.                 ',
     *   'SOURCE                  ', 'QUAL                    ',
     *   'CALCODE                 ', 'IFLUX                   ',
     *   'QFLUX                   ', 'UFLUX                   ',
     *   'VFLUX                   ',
     *   'FREQOFF                 ', 'BANDWIDTH               ',
     *   'RAEPO                   ', 'DECEPO                  ',
     *   'EPOCH                   ',
     *   'RAAPP                   ', 'DECAPP                  ',
     *   'RAOBS                   ', 'DECOBS                  ',
     *   'LSRVEL                  ', 'RESTFREQ                ',
     *   'PMRA                    ', 'PMDEC                   '/
      DATA UNITS /4*'        ', 'JY      ', 'JY      ',
     *    'JY      ', 'JY      ', 'HZ      ', 'HZ      ',
     *    'DEGREES ', 'DEGREES ',
     *   'YEARS   ',
     *   'DEGREES ', 'DEGREES ', 'DEGREES ', 'DEGREES ',
     *   'M/SEC   ', 'HZ      ', 'DEG/DAY ', 'DEG/DAY '/
      DATA KEYW /'NO_IF   ', 'VELTYP  ', 'VELDEF  ','FREQID  '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 20
      NCOL = MAXSUC
      NKEY = 4
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DO 10 I = 1,NDATA
            DATP(I,2) = DTYP(I)
 10         CONTINUE
        DATP(5,2) = DTYP(5) + 10 * NUMIF
        DATP(6,2) = DTYP(6) + 10 * NUMIF
        DATP(7,2) = DTYP(7) + 10 * NUMIF
        DATP(8,2) = DTYP(8) + 10 * NUMIF
        DATP(9,2) = DTYP(9) + 10 * NUMIF
        DATP(18,2) = DTYP(18) + 10 * NUMIF
        DATP(19,2) = DTYP(19) + 10 * NUMIF
        END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'SU', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SOUINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of sources
      ISURNO = BUFFER(5) + 1
      IF (DOREAD) ISURNO = 1
      NKEY = 4
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NDATA
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SOUINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'SOUINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. IF groups
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMIF
C                                       Velocity type
         KLOCS(2) = 2
         KEYTYP(2) = 3
         CALL CHR2H (8, VELTYP, 1, KEYVAL(KLOCS(2)))
C                                       Velocity definition
         KLOCS(3) = 4
         KEYTYP(3) = 3
         CALL CHR2H (8, VELDEF, 1, KEYVAL(KLOCS(3)))
C                                       Freqid of SU values
         KLOCS(4) = 6
         KEYTYP(4) = 4
         KEYVAL(KLOCS(4)) = FREQID
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'SOUINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'SOUINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. IF groups
         IPOINT = KLOCS(1)
         NUMIF = KEYVAL(IPOINT)
C                                       Velocity type
         IPOINT = KLOCS(2)
         VELTYP = '        '
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), VELTYP)
C                                       Velocity definition
         IPOINT = KLOCS(3)
         VELDEF = '        '
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), VELDEF)
C                                       FREQID
         FREQID = -999
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) FREQID = KEYVAL(IPOINT)
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'SOUINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'SU', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'SOUINI', IERR)
         GO TO 990
         END IF
C                                       Get array indices, element
C                                       counts
      CALL FNDCOL (NDATA, TITLE, 24, T, BUFFER, SUKOLS, IERR)
      IERR = 0
      DO 150 I = 1,NDATA
         IF (SUKOLS(I).GT.0) THEN
            IT = SUKOLS(I)
            SUKOLS(I) = DATP(IT,1)
            SUNUMV(I) = DATP(IT,2) / 10
            IF (SUNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            SUKOLS(I) = -1
            SUNUMV(I) = 0
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
 1100 FORMAT ('SUINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('SUINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('SUINI: ERROR INITIALIZING SOURCE TABLE FOR ',A4)
      END
