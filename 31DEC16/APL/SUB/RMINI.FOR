LOCAL INCLUDE 'PRMFIT.INC'
      INTEGER   MAXCMP, NCOLRM
      PARAMETER (MAXCMP=4)
      PARAMETER (NCOLRM=14)
LOCAL END
      SUBROUTINE RMINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IRMRNO, RMKOLS, RMNUMV, IBLC, ITRC, IYINC, IZINC, ICLIP,
     *   PCLIP, PDONE, SPIXDO, IRET)
C-----------------------------------------------------------------------
C! RMFIT answer/work table - rotation measure models
C# Table Extension Polarization
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2015
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
C   Creates and initializes Component fit (RM) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        RM file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      IBLC     I(2)     BLC of Y,Z plane
C      ITRC     I(2)     TRC of Y,Z plane
C      IYINC    I        increment in Y
C      IZINC    I        increment in Z
C      ICLIP    R        fit spectra only above I value ICLIP
C      PCLIP    R        fit spectra only above P value PCLIP
C      PDONE    I        number of planes with fit attempts
C      SPIXDO   I        Fitting spectral index (1), thickness (2)
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IRMRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      RMKOLS   I(MAXRMC)   The column pointer array in order:
C                        pixel(2), #comp, I average, P average,
C                        Pone(4), ThetaOne(4), rot.measure(4), spectral
C                        index(4), errors 4 of (4)
C      RMNUMV   I(MAXRMC)   Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   MAXRMC = 10 only known locally.
C-----------------------------------------------------------------------
      INTEGER   MAXRMK
      PARAMETER (MAXRMK = 10)
      INCLUDE 'PRMFIT.INC'
C
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IRMRNO,
     *   RMKOLS(NCOLRM), RMNUMV(NCOLRM), IBLC(2), ITRC(2), IYINC, IZINC,
     *   PDONE, SPIXDO, IRET
      REAL      ICLIP, PCLIP
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(NCOLRM)*24, UNITS(NCOLRM)*8,
     *   KEYW(MAXRMK)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(NCOLRM), NDATA,
     *   KLOCS(MAXRMK), KEYVAL(MAXRMK+2), KEYTYP(MAXRMK), IPOINT,
     *   MSGSAV, I, NC, JERR, JTRIM
      DOUBLE PRECISION KEYVAD(6), XX
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (KEYVAL, KEYVAD)
      DATA TTITLE /'AIPS ROTATION MEASURE FIT TABLE'/
      DATA DTYP /24, 14, 12, 12, 8*2, 2*22/
      DATA TITLE /'PIXEL', 'NCOMP', 'IFLUX', 'PFLUX', 'PONE',
     *   'THETA', 'ROT.MEAS', 'SPECINDX', 'ERR P1', 'ERR THET',
     *   'ERR RM', 'ERR SPIX', 'QU RMS ', 'WTED QU RMS'/
      DATA UNITS /'PIXELS', ' ', 2*'JY/BEAM', 'JY/BEAM', 'DEGREES',
     *   'RAD/M/M', ' ', 'JY/BEAM', 'DEGREES', 'RAD/M/M', ' ',
     *   2*'JY/BEAM'/
      DATA KEYW /'YBLC', 'YTRC', 'ZBLC', 'ZTRC', 'YINC', 'ZINC',
     *   'ICLIP', 'PCLIP', 'PIX FIT', 'DOSPIX'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = MAXRMK
      NDATA = NCOLRM
      NCOL = NDATA
      CALL FILL (NDATA, 0, RMKOLS)
      CALL FILL (NDATA, 0, RMNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         IF (SPIXDO.GE.2) THEN
            TITLE(8) = 'THICKNESS'
            TITLE(12) = 'ERR THICK'
            UNITS(8) = UNITS(7)
            UNITS(12) = UNITS(7)
            END IF
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 5 I = 5,4+2*MAXCMP
            DATP(I,2) = 10 * MAXCMP + 2
 5          CONTINUE
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'RM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'RMINI', IRET)
         GO TO 990
         END IF
      NEWFIL = IRET.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      IRMRNO = BUFFER(5) + 1
      IF (DOREAD) IRMRNO = 1
      NKEY = MAXRMK
C                                       File created, initialize
      IF (NEWFIL) THEN
         CALL H2CHR (8, 1, CATBLK(KHBUN), UNITS(3))
         UNITS(4) = UNITS(3)
         UNITS(5) = UNITS(3)
         UNITS(9) = UNITS(3)
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'RMINI', IRET)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'RMINI', IRET)
               GO TO 990
               END IF
 10         CONTINUE
C                                       Fill in Table title
         NTT = JTRIM (TTITLE)
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       yblc
         KLOCS(1) = 1
         KEYTYP(1) = 4
         KEYVAL(1) = IBLC(1)
C                                       ytrc
         KLOCS(2) = 2
         KEYTYP(2) = 4
         KEYVAL(2) = ITRC(1)
C                                       zblc
         KLOCS(3) = 3
         KEYTYP(3) = 4
         KEYVAL(3) = IBLC(2)
C                                       ztrc
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = ITRC(2)
C                                       yinc
         KLOCS(5) = 5
         KEYTYP(5) = 4
         KEYVAL(5) = IYINC
C                                       zinc
         KLOCS(6) = 6
         KEYTYP(6) = 4
         KEYVAL(6) = IZINC
C                                       I clip value
         KLOCS(7) = 7
         KEYTYP(7) = 1
         KEYVAD(4) = ICLIP
C                                       P clip value
         KLOCS(8) = 9
         KEYTYP(8) = 1
         KEYVAD(5) = PCLIP
C                                       number done
         PDONE = 0
         KLOCS(9) = 11
         KEYTYP(9) = 4
         KEYVAL(11) = PDONE
C                                       spectral index, thickness
         KLOCS(10) = 12
         KEYTYP(10) = 4
         KEYVAL(12) = SPIXDO
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'RMINI', IRET)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         MSGSUP = MSGSAV
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'RMINI', IRET)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       yblc
         IBLC(1) = 0
         IPOINT = KLOCS(1)
         IF (IPOINT.GT.0) IBLC(1) = KEYVAL(IPOINT)
C                                       ytrc
         ITRC(1) = 0
         IPOINT = KLOCS(2)
         IF (IPOINT.GT.0) ITRC(1) = KEYVAL(IPOINT)
C                                       zblc
         IBLC(2) = 0
         IPOINT = KLOCS(3)
         IF (IPOINT.GT.0) IBLC(2) = KEYVAL(IPOINT)
C                                       ztrc
         ITRC(2) = 0
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) ITRC(2) = KEYVAL(IPOINT)
C                                       yinc
         IYINC = 0
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) IYINC = KEYVAL(IPOINT)
C                                       zinc
         IZINC = 0
         IPOINT = KLOCS(6)
         IF (IPOINT.GT.0) IZINC = KEYVAL(IPOINT)
C                                       I clip level
         ICLIP = 0.0
         IPOINT = KLOCS(7)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            ICLIP = XX
            END IF
C                                       clip level
         PCLIP = 0.0
         IPOINT = KLOCS(8)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            PCLIP = XX
            END IF
C                                       pixels fits
         PDONE = 0
         IPOINT = KLOCS(9)
         IF (IPOINT.GT.0) PDONE = KEYVAL(IPOINT)
C                                       spectral index/thickness
         SPIXDO = 0
         IPOINT = KLOCS(10)
         IF (IPOINT.GT.0) SPIXDO = KEYVAL(IPOINT)
         IF (SPIXDO.GE.2) THEN
            TITLE(8) = 'THICKNESS'
            TITLE(12) = 'ERR THICK'
            UNITS(8) = UNITS(7)
            UNITS(12) = UNITS(7)
            END IF
         END IF
      IRET = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'RMINI', IRET)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'RM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'RMINI', IRET)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, RMKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = RMKOLS(I)
         IF (IPOINT.GT.0) THEN
            RMKOLS(I) = DATP(IPOINT,1)
            RMNUMV(I) = DATP(IPOINT,2) / 10
            IF (RMNUMV(I).LE.0) THEN
               NC = JTRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            RMKOLS(I) = -1
            RMNUMV(I) = 0
            NC = JTRIM (TITLE(I))
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
 1100 FORMAT ('RMINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('RMINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('RMINI: ERROR INITIALIZING RM-FIT TABLE FOR ',A)
      END
