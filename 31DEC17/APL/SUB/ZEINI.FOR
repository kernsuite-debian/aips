      SUBROUTINE ZEINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IZERNO, ZEKOLS, ZENUMV, IBLC, ITRC, DOGAUS, ICLIP, PDONE,
     *   REFVAL, REFPIX, REFINC, REFTYP, ABSORP, IRET)
C-----------------------------------------------------------------------
C! Fits Zeeman models to spectra: init ZE table used by ZEMAN
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2017
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
C   Creates and initializes Zeeman fit (ZE) extension tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use
C   Input/output
C      VER      I        ZE file version
C      CATBLK   I(256)   Catalog header block.
C   Input (create) / output (pre-existing)
C      IBLC     I(2)     BLC of Y,Z plane
C      ITRC     I(2)     TRC of Y,Z plane
C      DOGAUS   I        -1 2-sided derivative, 0 1-sided, 1 Gaussians
C      ICLIP    R        fit spectra only above ICLIP in I
C      PDONE    I        number of planes with fit attempts
C      REFVAL   D        Reference value for spectral axis
C      REFPIX   R        Reference pixel for spectral axis
C      REFVAL   R        Reference increment for spectral axis
C      REFVAL   C*8      Reference type for spectral axis
C      ABSORP   I        > 0 -> absorption line model: = 1 leakage gain
C                          applies to full Ipol(chan), = 2 leakage gain
C                          applies to Ipol(chan) - continuum (i.e. Vpol
C                          cube from continuum subtracted visibilities)
C   Output:
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      IZERNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      ZEKOLS   I(MAXZEC)   The column pointer array in order:
C                        pixel(2), I peak value, gain, gain error,
C                        field (4), field error(4), #gauss, amp(4),
C                        center(4), width(4)
C      ZENUMV   I(MAXZEC)   Element count in each column.
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   MAXZEC = 7 only known locally.
C-----------------------------------------------------------------------
      INTEGER   MAXZEC, MAXZEK, MAXGAU
      PARAMETER (MAXZEC = 11)
      PARAMETER (MAXZEK = 12)
      PARAMETER (MAXGAU = 8)
C
      CHARACTER OPCODE*4, REFTYP*8
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IZERNO,
     *   ZEKOLS(MAXZEC), ZENUMV(MAXZEC), IBLC(2), ITRC(2), DOGAUS,
     *   PDONE, ABSORP, IRET
      REAL      ICLIP, REFPIX, REFINC
      DOUBLE PRECISION REFVAL
C
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE(MAXZEC)*24, UNITS(MAXZEC)*8,
     *   KEYW(MAXZEK)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXZEC), NDATA,
     *   KLOCS(MAXZEK), KEYVAL(MAXZEK+6), KEYTYP(MAXZEK), IPOINT,
     *   MSGSAV, I, NC, JERR, JTRIM, II
      REAL      RR
      DOUBLE PRECISION KEYVAD(9), XX
      LOGICAL   DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (KEYVAL, KEYVAD), (II, RR)
      DATA TTITLE /'AIPS ZEMAN FIT TABLE'/
      DATA DTYP /24, 12, 2*12, 2*0, 14, 3*0, 22/
      DATA TITLE /'PIXEL', 'I PEAK', 'GAIN', 'ERR GAIN',
     *   'B  FIELD', 'ERR BFLD', 'NGAUS', 'AMPLITUD', 'CENTER', 'WIDTH',
     *   'BASELINE'/
      DATA UNITS /'PIXELS', 'JY/BEAM', 2 * ' ', 2*'PIXELS', ' ',
     *   'JY/BEAM', 2*'PIXELS', 'JY/BEAM'/
      DATA KEYW /'YBLC', 'YTRC', 'ZBLC', 'ZTRC', 'DOGAUS', 'PIX FIT',
     *   'ICLIP', 'REFVALUE', 'REFPIXEL', 'REFINCR', 'REFTYPE',
     *   'ABSORPTN'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 500
      NKEY = MAXZEK
      NDATA = MAXZEC
      NCOL = NDATA
      CALL FILL (NDATA, 0, ZEKOLS)
      CALL FILL (NDATA, 0, ZENUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) THEN
         CALL COPY (NDATA, DTYP, DATP(1,2))
         DO 5 I = 5,10
            IF (DATP(I,2).LE.0) DATP(I,2) = 10 * MAXGAU + 2
 5          CONTINUE
         END IF
      CALL FNDEXT ('ZE', CATBLK, I)
      IF ((I.GT.0) .AND. (VER.GT.0) .AND. (VER.LE.I)) THEN
         NCOL = 0
         NKEY = 0
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'ZE', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ZEINI', IRET)
         GO TO 990
         END IF
      NEWFIL = IRET.LT.0
      MSGSAV = MSGSUP
C                                       Get number of scans
      IZERNO = BUFFER(5) + 1
      IF (DOREAD) IZERNO = 1
      NKEY = MAXZEK
C                                       File created, initialize
      IF (NEWFIL) THEN
         CALL H2CHR (8, 1, CATBLK(KHBUN), UNITS(3))
C                                       Col. labels.
         DO 10 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ZEINI', IRET)
               GO TO 990
               END IF
C                                       Units
            IF ((I.EQ.8) .AND. (ABSORP.GT.0)) THEN
               CALL CHR2H (8, 'OptDepth', 1, HOLTMP)
            ELSE
               CALL CHR2H (8, UNITS(I), 1, HOLTMP)
               END IF
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ZEINI', IRET)
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
C                                       dogaus
         KLOCS(5) = 5
         KEYTYP(5) = 4
         KEYVAL(5) = DOGAUS
C                                       number done
         PDONE = 0
         KLOCS(6) = 6
         KEYTYP(6) = 4
         KEYVAL(7) = PDONE
C                                       clip value
         KLOCS(7) = 7
         KEYTYP(7) = 1
         KEYVAD(4) = ICLIP
C                                       ref value
         KLOCS(8) = 9
         KEYTYP(8) = 1
         KEYVAD(5) = REFVAL
C                                       ref pixel
         KLOCS(9) = 11
         KEYTYP(9) = 1
         KEYVAD(6) = REFPIX
C                                       ref incr
         KLOCS(10) = 13
         KEYTYP(10) = 1
         KEYVAD(7) = REFINC
C                                       ref type
         KLOCS(11) = 15
         KEYTYP(11) = 3
         CALL CHR2H (8, REFTYP, 1, KEYVAL(15))
C                                       absorption
         KLOCS(12) = 17
         KEYTYP(12) = 4
         KEYVAL(17) = ABSORP
C                                       Only write if just created.
         CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'ZEINI', IRET)
            GO TO 990
            END IF
C                                       Read keywords
      ELSE
         MSGSUP = 32000
         CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IRET)
         MSGSUP = MSGSAV
         IF ((IRET.GE.1) .AND. (IRET.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'ZEINI', IRET)
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
C                                       ytrc
         ITRC(2) = 0
         IPOINT = KLOCS(4)
         IF (IPOINT.GT.0) ITRC(2) = KEYVAL(IPOINT)
C                                       DOGAUS
         DOGAUS = 1
         IPOINT = KLOCS(5)
         IF (IPOINT.GT.0) DOGAUS = KEYVAL(IPOINT)
C                                       pixels fit
         PDONE = 0
         IPOINT = KLOCS(6)
         IF (IPOINT.GT.0) PDONE = KEYVAL(IPOINT)
C                                       clip level
         ICLIP = 0.0
         IPOINT = KLOCS(7)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            ICLIP = XX
            END IF
C                                       ref value
         CALL RCOPY (NWDPDP, CATBLK(2*KDCRV-1), XX)
         IPOINT = KLOCS(8)
         IF (IPOINT.GT.0) CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
         REFVAL = XX
C                                       ref pixel
         IPOINT = KLOCS(9)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            REFPIX = XX
         ELSE
            II = CATBLK(KRCRP)
            REFPIX = RR
            END IF
C                                       ref incr
         IPOINT = KLOCS(10)
         IF (IPOINT.GT.0) THEN
            CALL RCOPY (NWDPDP, KEYVAL(IPOINT), XX)
            REFINC = XX
         ELSE
            II = CATBLK(KRCIC)
            REFINC = RR
            END IF
C                                       ref type
         IPOINT = KLOCS(11)
         IF (IPOINT.GT.0) THEN
            CALL H2CHR (8, 1, KEYVAL(IPOINT), REFTYP)
         ELSE
            CALL H2CHR (8, 1, CATBLK(KHCTP), REFTYP)
            END IF
C                                       absorption
         ABSORP = 0
         IPOINT = KLOCS(12)
         IF (IPOINT.GT.0) ABSORP = KEYVAL(IPOINT)
         END IF
      IRET = 0
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KEYVAL, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'ZEINI', IRET)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'ZE', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ZEINI', IRET)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, TITLE, 24, .TRUE., BUFFER, ZEKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = ZEKOLS(I)
         IF (IPOINT.GT.0) THEN
            ZEKOLS(I) = DATP(IPOINT,1)
            ZENUMV(I) = DATP(IPOINT,2) / 10
            IF (ZENUMV(I).LE.0) THEN
               NC = JTRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            ZEKOLS(I) = -1
            ZENUMV(I) = 0
            NC = JTRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (I.NE.11) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ZEINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('ZEINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('ZEINI: ERROR INITIALIZING GAUSS-FIT TABLE FOR ',A)
      END
