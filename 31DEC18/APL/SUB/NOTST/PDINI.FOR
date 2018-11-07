      SUBROUTINE PDINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IPDRNO, PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   POLTYP, IERR)
C-----------------------------------------------------------------------
C! Create/open/initialize antenna polarization spectrum (PD) table
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2012
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
C   Creates and initializes antenna polarization spectrum (PD) extension
C   tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        PD file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      NUMANT   I        Number of antennas
C      NUMPOL   I        Number of polarizations.
C      NUMIF    I        Number of IFs
C      NUMFRQ   I        Number of frequency channels
C      POLTYP   C*8      Polarization solution type:
C                           'APPROX','ORI-ELP','X-Y LIN', 'VLBI'
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IPDRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      PDKOLS   I(MAXPDC)   The column pointer array in order:
C                        ANTENNA, SUBARRAY, FREQ ID,
C                        REFANT, P_DIFF
C                        REAL 1, IMAG 1, REAL 2, IMAG 2
C     PDNUMV    I(MAXPDC)   Element count in each column.
C     IERR      I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   MAXPDC = 9 only known locally.
C-----------------------------------------------------------------------
C     INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPDC
      PARAMETER (MAXPDC = 9)
C
      CHARACTER OPCODE*4, POLTYP*8
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IPDRNO,
     *   PDKOLS(MAXPDC), PDNUMV(MAXPDC), NUMANT, NUMPOL, NUMIF, NUMFRQ,
     *   IERR
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXPDC)*24, UNITS(MAXPDC)*8, KEYW(5)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXPDC), NDATA,
     *   KLOCS(5), KEYVAL(6), KEYTYP(5), IPOINT, MSGSAV, I, NC, JERR,
     *   ITRIM
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NTT /56/
      DATA TTITLE /'AIPS ANTENNA POLARIZATION SPECTRUM TABLE'/
      DATA DTYP /4*14, 5*2/
      DATA TITLE /'ANTENNA', 'SUBARRAY ', 'FREQ ID ', 'REFANT',
     *   'P_DIFF', 'REAL 1 ', 'IMAG 1 ', 'REAL 2 ', 'IMAG 2 '/
      DATA UNITS /9*' '/
      DATA KEYW /'NO_ANT', 'NO_POL', 'NO_IF', 'NO_CHAN', 'POLTYPE'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = 5
      NDATA = MAXPDC
      CALL FILL (NDATA, 0, PDKOLS)
      CALL FILL (NDATA, 0, PDNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         NCOL = 5 + NUMPOL * 2
         CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Correct for real/imag
         DATP(5,2) = DTYP(5)
         IF (POLTYP.EQ.'ORI-ELP') DATP(5,2) = DTYP(5) + 10*NUMFRQ*NUMIF
         DATP(6,2) = DTYP(6) + 10*NUMFRQ*NUMIF
         DATP(7,2) = DTYP(7) + 10*NUMFRQ*NUMIF
         DATP(8,2) = DTYP(8) + 10*NUMFRQ*NUMIF
         DATP(9,2) = DTYP(9) + 10*NUMFRQ*NUMIF
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'PD', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'PDINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      IPDRNO = BUFFER(5) + 1
      IF (DOREAD) IPDRNO = 1
      NKEY = 5
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'PDINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'PDINI', IERR)
               GO TO 990
               END IF
 10         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       No. antennas.
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = NUMANT
C                                       No. Polarizations
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = NUMPOL
C                                       No. IFs
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = NUMIF
C                                       No. CHAN
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = NUMFRQ
C                                       polarization solution type
         KLOCS(5) = 5
         KEYTYP(5) = 3
         CALL CHR2H (8, POLTYP, 1, KEYVAL(5))
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'PDINI', IERR)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         MSGSUP = MSGSAV
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'PDINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       No. antennas.
         NUMANT = 0
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. polarizations
         NUMPOL = 0
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IFs
         NUMIF = 0
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       No. CHAN
         NUMFRQ = 0
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) NUMFRQ = KEYVAL(IPOINT)
C                                       polarization solution type
         POLTYP = ' '
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) CALL H2CHR (8, 1, KEYVAL(IPOINT), POLTYP)
         END IF
      IERR = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'PDINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'PD', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'PDINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, PDKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = PDKOLS(I)
         IF (IPOINT.GT.0) THEN
            PDKOLS(I) = DATP(IPOINT,1)
            PDNUMV(I) = DATP(IPOINT,2) / 10
            IF (PDNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF ((I.NE.5) .OR. (POLTYP.EQ.'ORI-ELP')) CALL MSGWRT (6)
               END IF
         ELSE
            PDKOLS(I) = -1
            PDNUMV(I) = 0
            IF ((NUMPOL.GT.1) .OR. (I.LE.NDATA-2)) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1101) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PDINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('PDINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('PDINI: ERROR INITIALIZING POLARIZATION TABLE FOR ',A)
      END
