      SUBROUTINE XGINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IXGRNO, XGKOLS, XGNUMV, IBLC, ITRC, IYINC, IZINC, VCLIP,
     *   VOFF, PDONE, REFVAL, REFPIX, REFINC, REFTYP, ABSORP, IRET)
C-----------------------------------------------------------------------
C! Init table IO for XGAUS results
C# Map Spectral Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2013, 2016-2017
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
C   Creates and initializes Gaussian fit (XG) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        XG file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      IBLC     I(2)     BLC of Y,Z plane
C      ITRC     I(2)     TRC of Y,Z plane
C      IYINC    I        increment in Y
C      IZINC    I        increment in Z
C      VCLIP    R        fit spectra only above VCLIP
C      VOFF     D        central value in X axis for offset
C      PDONE    I        number of planes with fit attempts
C      REFVAL   D        Reference value for spectral axis
C      REFPIX   R        Reference pixel for spectral axis
C      REFVAL   R        Reference increment for spectral axis
C      REFTYP   C*8      Reference type for spectral axis
C      ABSORP   I        1 -> absorption, <=0 -> emission
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IXGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      XGKOLS   I(MAXXGC)   The column pointer array in order:
C                        pixel(2), #gauss, peak value, baseline(2),
C                        baseline error(2), amp(4), center(4), width(4),
C                        amp error(4), center error(4), width error(4)
C      XGNUMV   I(MAXXGC)   Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   MAXXGC = 11 only known locally.
C-----------------------------------------------------------------------
      INTEGER   MAXXGC, MAXXGK, MAXGAU
      PARAMETER (MAXXGC = 11)
      PARAMETER (MAXXGK = 14)
      PARAMETER (MAXGAU = 8)
C
      CHARACTER OPCODE*4, REFTYP*8
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IXGRNO,
     *   XGKOLS(MAXXGC), XGNUMV(MAXXGC), IBLC(2), ITRC(2), IYINC, IZINC,
     *   PDONE, ABSORP, IRET
      REAL      VCLIP, REFPIX, REFINC
      DOUBLE PRECISION VOFF, REFVAL
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXXGC)*24, UNITS(MAXXGC)*8,
     *   KEYW(MAXXGK)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXXGC), NDATA,
     *   KLOCS(MAXXGK), KEYVAL(MAXXGK+6), KEYTYP(MAXXGK), IPOINT,
     *   MSGSAV, I, NC, JERR, JTRIM, II
      REAL      RR
      DOUBLE PRECISION KEYVAD(10), XX
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (KEYVAL, KEYVAD), (RR, II)
      DATA TTITLE /'AIPS XGAUSS GAUSSIAN FIT TABLE'/
      DATA DTYP /24, 14, 12, 22, 22, 6*2/
      DATA TITLE /'PIXEL', 'NGAUS', 'PEAK', 'BASELINE', 'ERR BASE',
     *   'AMPLITUD', 'CENTER', 'WIDTH', 'ERR AMPL', 'ERR CENT',
     *   'ERR WIDT'/
      DATA UNITS /11*'PIXELS'/
      DATA KEYW /'YBLC', 'YTRC', 'ZBLC', 'ZTRC', 'YINC', 'ZINC',
     *   'VCLIP', 'VOFF', 'REFVALUE', 'REFPIXEL', 'REFINCR', 'REFTYPE',
     *   'ABSORPTN', 'PIX FIT'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = MAXXGK
      NDATA = MAXXGC
      NCOL = NDATA
      CALL FILL (NDATA, 0, XGKOLS)
      CALL FILL (NDATA, 0, XGNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 5 I = 6,11
            DATP(I,2) = 10 * MAXGAU + 2
 5          CONTINUE
      ELSE
         NCOL = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'XG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'XGINI', IRET)
         GO TO 990
         END IF
      NEWFIL = IRET.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      IXGRNO = BUFFER(5) + 1
      IF (DOREAD) IXGRNO = 1
      NKEY = MAXXGK
C                                       File created, initialize
      IF (NEWFIL) THEN
         CALL H2CHR (8, 1, CATBLK(KHBUN), UNITS(3))
         UNITS(4) = UNITS(3)
         IF (ABSORP.LE.0) THEN
            UNITS(6) = UNITS(3)
         ELSE
            UNITS(6) = 'OptDepth'
            END IF
         UNITS(9) = UNITS(6)
         UNITS(2) = ' '
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'XGINI', IRET)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'XGINI', IRET)
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
C                                       xtrc
         KLOCS(4) = 4
         KEYTYP(4) = 4
         KEYVAL(4) = ITRC(2)
C                                       yinc
         KLOCS(5) = 5
         KEYTYP(5) = 4
         KEYVAL(5) = IYINC
C                                       xinc
         KLOCS(6) = 6
         KEYTYP(6) = 4
         KEYVAL(6) = IZINC
C                                       clip value
         KLOCS(7) = 7
         KEYTYP(7) = 1
         KEYVAD(4) = VCLIP
C                                       offset value
         KLOCS(8) = 9
         KEYTYP(8) = 1
         KEYVAD(5) = VOFF
C                                       ref value
         KLOCS(9) = 11
         KEYTYP(9) = 1
         KEYVAD(6) = REFVAL
C                                       ref pixel
         KLOCS(10) = 13
         KEYTYP(10) = 1
         KEYVAD(7) = REFPIX
C                                       ref incr
         KLOCS(11) = 15
         KEYTYP(11) = 1
         KEYVAD(8) = REFINC
C                                       ref type
         KLOCS(12) = 17
         KEYTYP(12) = 3
         CALL CHR2H (8, REFTYP, 1, KEYVAL(17))
C                                       absorption
         KLOCS(13) = 19
         KEYTYP(13) = 4
         KEYVAL(19) = ABSORP
C                                       number done
         PDONE = 0
         KLOCS(14) = 20
         KEYTYP(14) = 4
         KEYVAL(20) = PDONE
C
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'XGINI', IRET)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         MSGSUP = MSGSAV
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'XGINI', IRET)
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
C                                       clip level
         VCLIP = 0.0
         IPOINT = KLOCS(7)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            VCLIP = XX
            END IF
C                                       offset
         VOFF = 0.0D0
         IPOINT = KLOCS(8)
         IF (IPOINT.GT.0) CALL RCOPY (NWDPDP, KEYVAL(IPOINT), VOFF)
C                                       pixels fit
         PDONE = 0
         IPOINT = KLOCS(14)
         IF (IPOINT.GT.0) PDONE = KEYVAL(IPOINT)
C                                       ref value
         CALL RCOPY (NWDPDP, CATBLK(2*KDCRV-1), XX)
         IPOINT = KLOCS(9)
         IF (IPOINT.GT.0) CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
         REFVAL = XX
C                                       ref pixel
         IPOINT = KLOCS(10)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            REFPIX = XX
         ELSE
            II = CATBLK(KRCRP)
            REFPIX = RR
            END IF
C                                       ref incr
         IPOINT = KLOCS(11)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            REFINC = XX
         ELSE
            II = CATBLK(KRCIC)
            REFINC = RR
            END IF
C                                       ref type
         IPOINT = KLOCS(12)
         IF (IPOINT.GT.0) THEN
            CALL H2CHR (8, 1, KEYVAL(IPOINT), REFTYP)
         ELSE
            CALL H2CHR (8, 1, CATBLK(KHCTP), REFTYP)
            END IF
C                                       absorption
         IPOINT = KLOCS(13)
         IF (IPOINT.GT.0) THEN
            ABSORP = KEYVAL(IPOINT)
         ELSE
            ABSORP = 0
            END IF
         END IF
      IRET = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'XGINI', IRET)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'XG', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'XGINI', IRET)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, XGKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = XGKOLS(I)
         IF (IPOINT.GT.0) THEN
            XGKOLS(I) = DATP(IPOINT,1)
            XGNUMV(I) = DATP(IPOINT,2) / 10
            IF (XGNUMV(I).LE.0) THEN
               NC = JTRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            XGKOLS(I) = -1
            XGNUMV(I) = 0
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
 1100 FORMAT ('XGINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('XGINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('XGINI: ERROR INITIALIZING GAUSS-FIT TABLE FOR ',A)
      END
