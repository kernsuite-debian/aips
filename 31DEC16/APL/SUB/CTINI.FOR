      SUBROUTINE CTINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICTRNO, CTKOLS, CTNUMV, IERR)
C-----------------------------------------------------------------------
C! creates and initializes Calc (CT) tables
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2005-2007
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
C   Creates and initializes Calc tables.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C   Input (create only) / output (pre-existing file):
C      VER      I        AN file version, actual value set on return
C   Input/output common defined by DCTV.INC - header keywords
C   Output:
C      ICTRNO   I        Next row number, start of the file if READ,
C                        the last+1 if WRITE
C      CTKOLS   I(12)    Column pointers
C      CTNUMV   I(12)    Number values in each column
C      IERR     I        Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C   Useage NOTE: use the include 'DCTV.INC' for the declarations in
C   CTINI and TABCT.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, ICTRNO,
     *   CTKOLS(*), CTNUMV(*), IERR
C
      INCLUDE 'INCS:DCTV.INC'
C
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXCTC), I,
     *   NDATA, KLOCS(NKEYCT), KEYVAL(NKYWCT), IPOINT, KEYTYP(NKEYCT),
     *   ITEMP(6), NKEYRD, MSGSAV, JERR, NUMV(MAXCTC), NC, ITRIM
      LOGICAL   DOREAD, NEWFIL, DOMSG
      REAL      KEYVAR(NKYWCT)
      HOLLERITH KEYVAH(NKYWCT), HOLTMP(6)
      CHARACTER KEYW(NKEYCT)*8, TTITLE*56, TITLE(MAXCTC)*24,
     *   UNITS(MAXCTC)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH),  (HOLTMP, ITEMP)
      DATA TTITLE /'AIPS UV DATA FILE CALC TABLE '/
      DATA DTYP /1,1,1,1,3,1,3,1,1,1,1,1/
      DATA TITLE /'TIME', 'UT1-UTC', 'IAT-UTC', 'A1-IAT', 'UT1 TYPE',
     *   'WOBXY', 'WOB TYPE', 'DPSI', 'DDPSI', 'DEPS', 'DDEPS',
     *   'TIME INT'/
      DATA UNITS /'DAYS', 'SECONDS', 'SECONDS', 'SECONDS', ' ',
     *   'ARCSEC', ' ', 'RADIANS', 'RAD/SEC', 'RADIANS', 'RAD/SEC',
     *   'DAYS'/
      DATA NUMV /1,1,1,1,1,2,1,1,1,1,1,2/
      DATA KEYW /'OBSCODE', 'RDATE', 'NO_STKD', 'STK_1', 'NO_BAND',
     *   'NO_CHAN', 'REF_FREQ', 'CHAN_BW', 'REF_PIXL', 'TABREV',
     *   'C_SRVR', 'C_VERSN', 'A_VERSN', 'I_VERSN', 'E_VERSN',
     *   'ACCELGRV', 'E-FLAT', 'EARTHRAD', 'MMSEMS', 'EPHEPOC',
     *   'ETIDELAG', 'GAUSS', 'GMMOON', 'GMSUN', 'LOVE_H', 'LOVE_L',
     *   'PRE_DATA', 'REL_DATA', 'TIDALUT1', 'TSECAU', 'U-GRV-CN',
     *   'VLIGHT'/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 30
      NCOL = MAXCTC
      NDATA = MAXCTC
      NKEY = NKEYCT
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DO 10 I = 1,NDATA
            CTNUMV(I) = NUMV(I)
            DATP(I,2) = DTYP(I) + CTNUMV(I) * 10
 10         CONTINUE
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'CT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CTINI', IERR)
         GO TO 990
         END IF
      NEWFIL = (IERR.LT.0)
      NKEYRD = BUFFER(53)
C                                       Get number of records
      ICTRNO = BUFFER(5) + 1
      IF (DOREAD) ICTRNO = 1
      NKEY = NKEYCT
C                                       File created, initialize
      IF (NEWFIL) THEN
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CTINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'CTINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         NTT = ITRIM (TTITLE)
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
         IPOINT = 1
C                                       'OBSCODE', 'RDATE'
         KLOCS(1) = IPOINT
         KEYTYP(1) = 3
         CALL CHR2H (8, CTOBSC, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         KLOCS(2) = IPOINT
         KEYTYP(2) = 3
         CALL CHR2H (8, CTDATE, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       'NO_STKD', 'STK_1', 'NO_BAND',
C                                       'NO_CHAN'
         KLOCS(3) = IPOINT
         KEYTYP(3) = 4
         KEYVAL(IPOINT) = CTNSTK
         IPOINT = IPOINT + 1
         KLOCS(4) = IPOINT
         KEYTYP(4) = 4
         KEYVAL(IPOINT) = CTSTK1
         IPOINT = IPOINT + 1
         KLOCS(5) = IPOINT
         KEYTYP(5) = 4
         KEYVAL(IPOINT) = CTNIF
         IPOINT = IPOINT + 1
         KLOCS(6) = IPOINT
         KEYTYP(6) = 4
         KEYVAL(IPOINT) = CTNCHN
         IPOINT = IPOINT + 1
C                                       'REF_FREQ', 'CHAN_BW', 'REF_PIXL'
         KLOCS(7) = IPOINT
         KEYTYP(7) = 1
         CALL RCOPY (NWDPDP, CTRFRQ, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(8) = IPOINT
         KEYTYP(8) = 1
         CALL RCOPY (NWDPDP, CTCBW, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(9) = IPOINT
         KEYTYP(9) = 1
         CALL RCOPY (NWDPDP, CTRPIX, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'TABREV'
         KLOCS(10) = IPOINT
         KEYTYP(10) = 4
         KEYVAL(IPOINT) = CTTABV
C                                       force new rev number
         KEYVAL(IPOINT) = ICTREV
         IPOINT = IPOINT + 1
C                                       'C_SRVR', 'C_VERSN', 'A_VERSN',
         KLOCS(11) = IPOINT
         KEYTYP(11) = 3
         CALL CHR2H (8, CTSRV, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         KLOCS(12) = IPOINT
         KEYTYP(12) = 3
         CALL CHR2H (8, CTCVER, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         KLOCS(13) = IPOINT
         KEYTYP(13) = 3
         CALL CHR2H (8, CTAVER, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       'I_VERSN', 'E_VERSN',
         KLOCS(14) = IPOINT
         KEYTYP(14) = 3
         CALL CHR2H (8, CTIVER, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
         KLOCS(15) = IPOINT
         KEYTYP(15) = 3
         CALL CHR2H (8, CTEVER, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       'ACCELGRV', 'E-FLAT',
         KLOCS(16) = IPOINT
         KEYTYP(16) = 1
         CALL RCOPY (NWDPDP, CTACCG, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(17) = IPOINT
         KEYTYP(17) = 1
         CALL RCOPY (NWDPDP, CTEFLA, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'EARTHRAD', 'MMSEMS'
         KLOCS(18) = IPOINT
         KEYTYP(18) = 1
         CALL RCOPY (NWDPDP, CTERAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(19) = IPOINT
         KEYTYP(19) = 1
         CALL RCOPY (NWDPDP, CTMMSE, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'EPHEPOC'
         KLOCS(20) = IPOINT
         KEYTYP(20) = 4
         KEYVAL(IPOINT) = CTEPO
         IPOINT = IPOINT + 1
C                                       'ETIDELAG', 'GAUSS', 'GMMOON',
         KLOCS(21) = IPOINT
         KEYTYP(21) = 1
         CALL RCOPY (NWDPDP, CTETID, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(22) = IPOINT
         KEYTYP(22) = 1
         CALL RCOPY (NWDPDP, CTGAUS, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(23) = IPOINT
         KEYTYP(23) = 1
         CALL RCOPY (NWDPDP, CTGMMO, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'GMSUN', 'LOVE_H', 'LOVE_L',
         KLOCS(24) = IPOINT
         KEYTYP(24) = 1
         CALL RCOPY (NWDPDP, CTGMDU, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(25) = IPOINT
         KEYTYP(25) = 1
         CALL RCOPY (NWDPDP, CTLOVH, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(26) = IPOINT
         KEYTYP(26) = 1
         CALL RCOPY (NWDPDP, CTLOVL, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'PRE_DATA', 'REL_DATA'
         KLOCS(27) = IPOINT
         KEYTYP(27) = 1
         CALL RCOPY (NWDPDP, CTPDAT, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(28) = IPOINT
         KEYTYP(28) = 1
         CALL RCOPY (NWDPDP, CTRDAT, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       'TIDALUT1'
         KLOCS(29) = IPOINT
         KEYTYP(29) = 4
         KEYVAL(IPOINT) = CTTID1
         IPOINT = IPOINT + 1
C                                       'TSECAU', 'U-GRV-CN', 'VLIGHT'
         KLOCS(30) = IPOINT
         KEYTYP(30) = 1
         CALL RCOPY (NWDPDP, CTTSEC, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(31) = IPOINT
         KEYTYP(31) = 1
         CALL RCOPY (NWDPDP, CTGRVC, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(32) = IPOINT
         KEYTYP(32) = 1
         CALL RCOPY (NWDPDP, CTLIGH, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *      IERR)
         NKEYRD = NKEY
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'CTINI', IERR)
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
            CALL TABERR ('READ', 'TABKEY', 'CTINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       'OBSCODE', 'RDATE'
         IPOINT = KLOCS(1)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTOBSC)
         IPOINT = KLOCS(2)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTDATE)
C                                       'NO_STKD', 'STK_1', 'NO_BAND',
C                                       'NO_CHAN'
         IPOINT = KLOCS(3)
         CTNSTK = KEYVAL(IPOINT)
         IPOINT = KLOCS(4)
         CTSTK1 = KEYVAL(IPOINT)
         IPOINT = KLOCS(5)
         CTNIF  = KEYVAL(IPOINT)
         IPOINT = KLOCS(6)
         CTNCHN = KEYVAL(IPOINT)
C                                       'REF_FREQ', 'CHAN_BW', 'REF_PIXL'
         IPOINT = KLOCS(7)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTRFRQ)
         IPOINT = KLOCS(8)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTCBW)
         IPOINT = KLOCS(9)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTRPIX)
C                                       'TABREV'
         IPOINT = KLOCS(10)
         CTTABV = KEYVAL(IPOINT)
C                                       'C_SRVR', 'C_VERSN', 'A_VERSN',
         IPOINT = KLOCS(11)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTSRV)
         IPOINT = KLOCS(12)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTCVER)
         IPOINT = KLOCS(13)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTAVER)
C                                       'I_VERSN', 'E_VERSN',
         IPOINT = KLOCS(14)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTIVER)
         IPOINT = KLOCS(15)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), CTEVER)
C                                       'ACCELGRV', 'E-FLAT',
         IPOINT = KLOCS(16)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTACCG)
         IPOINT = KLOCS(17)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTEFLA)
C                                       'EARTHRAD', 'MMSEMS'
         IPOINT = KLOCS(18)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTERAD)
         IPOINT = KLOCS(19)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTMMSE)
C                                       'EPHEPOC'
         IPOINT = KLOCS(20)
         CTEPO = KEYVAL(IPOINT)
C                                       'ETIDELAG', 'GAUSS', 'GMMOON',
         IPOINT = KLOCS(21)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTETID)
         IPOINT = KLOCS(22)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTGAUS)
         IPOINT = KLOCS(23)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTGMMO)
C                                       'GMSUN', 'LOVE_H', 'LOVE_L',
         IPOINT = KLOCS(24)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTGMDU)
         IPOINT = KLOCS(25)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTLOVH)
         IPOINT = KLOCS(26)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTLOVL)
C                                       'PRE_DATA', 'REL_DATA'
         IPOINT = KLOCS(27)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTPDAT)
         IPOINT = KLOCS(28)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTRDAT)
C                                       'TIDALUT1'
         IPOINT = KLOCS(29)
         CTTID1 = KEYVAL(IPOINT)
C                                       'TSECAU', 'U-GRV-CN', 'VLIGHT'
         IPOINT = KLOCS(30)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTTSEC)
         IPOINT = KLOCS(31)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTGRVC)
         IPOINT = KLOCS(32)
         CALL RCOPY (NWDPDP, KEYVAR(IPOINT), CTLIGH)
         END IF
      IERR = 0
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, ITEMP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'CTINI', IERR)
         GO TO 980
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'CT', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'CTINI', IERR)
         GO TO 980
         END IF
C                                       Get array indices
      CALL FNDCOL (MAXCTC, TITLE, 24, .TRUE., BUFFER, CTKOLS, JERR)
C                                       Get array indices and no. values
      DOMSG = (TSKNAM.NE.'FITLD') .OR. (NCOL.GE.MAXCTC)
      DO 150 I = 1,MAXCTC
         IPOINT = CTKOLS(I)
         IF (IPOINT.GT.0) THEN
            CTKOLS(I) = DATP(IPOINT,1)
            CTNUMV(I) = DATP(IPOINT,2) / 10
            IF (CTNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            CTKOLS(I) = -1
            CTNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF ((DOMSG) .OR. (I.NE.MAXCTC)) CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       close on error
 980  CALL TABIO ('CLOS', 0, 1, BUFFER, BUFFER, I)
C                                       Error
 990  WRITE (MSGTXT,1990) IERR, OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CTINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('CTINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('CTINI: ERROR',I4,' INITIALIZING CALC TABLE FOR ',A)
      END
