      SUBROUTINE ANINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
C-----------------------------------------------------------------------
C! creates and initializes antenna tables
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2005-2006, 2008-2010
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
C   Creates and initializes antenna tables.  THIS ROUTINE SHOULD BE
C   CALLED BY ANTINI ONLY - DO NOT CALL IT FROM YOUR OWN CODE.
C   This does the actual file creation and inits the I/O for read or
C   write - but ANTINI takes care of any needed format translation as
C   well.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                           'WRIT' = create/init for write or read
C                           'READ' = open for read only
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(*)     Catalog header block.
C      LUN      I        Logical unit number to use
C   Input (create only) / output (pre-existing file):
C      VER      I        AN file version, actual value set on return
C      ARRAYC   D(3)     Array center X coord. (meters, earth center)
C      GSTIA0   D        GST at time=0 (degrees) on ref. date
C      DEGPDY   D        Earth rotation rate (deg/IAT day)
C      SAFREQ   D        Obs. Reference Frequency for subarray(Hz)
C      RDATE    C*8      Reference date as 'DD/MM/YY'
C      POLRXY   R(2)     Polar position X,Y (src sec) on ref. date
C      UT1UTC   R        UT1-UTC  (time sec.)          "
C      DATUTC   R        data time-UTC  (time sec.)          "
C      TIMSYS   C*8      Time system, 'IAT' or 'UTC'
C      ANAME    C*8      Array name
C      XYZHAN   C*8      Array handedness ('RIGHT' will be forced)
C      TFRAME   C*8      Terrestrial coordinate frame
C      NUMORB   I        Number of orbital parameters
C      NOPCAL   I        Number of polarization calibration constants.
C      ANTNIF   I        Number of IFs.
C      ANFQID   I        Table keyword, denotes the FQ ID for which
C                          the AN parms have been modified. On O/P if
C                          ANFQID = -999 it is not in the table, if
C                          ANFQID = -1 the virgin values still exist,
C                          or the data have no FREQID random parameter
C   Output:
C      IANRNO   I        Next scan number, start of the file if READ,
C                        the last+1 if WRITE
C      ANNUMV   I(*)     Element count in each column size MAXANC
C      ANKOLS   I(*)     The column pointer array in order, ANNAME,
C                        STABXYZ, ORBPARM, NOSTA, MNTSTA, STAXOF,
C                        POLTYA, POLAA, POLCALA, POLTYB, POLAB, POLCALB
C                        - size MAXANC
C      IERR     I        Return error code, 0=>OK, else error
C   Useage NOTE: use the include 'DANT.INC' for the declarations in
C   ANINI and TABAN.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C                                       Any changes here should also be
C                                       made in DANT.INC and ANTINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), NUMORB, NOPCAL, IANRNO,
     *   ANTNIF, ANFQID
      CHARACTER ANAME*8, TIMSYS*8, RDATE*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN, IERR
C
      INTEGER   NK
      PARAMETER (NK = 20)
C
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXANC), NDATA,
     *   KLOCS(NK), KEYVAL(80), IPOINT, KEYTYP(NK), I, ITEMP(6), NKEYRD,
     *   MSGSAV, JERR, NC, ITRIM
      LOGICAL   DOREAD, NEWFIL
      REAL      KEYVAR(80), XYPOL(2), RADSQ
      HOLLERITH KEYVAH(80), HOLTMP(6)
      DOUBLE PRECISION KEYVAD, JD, UTC, SECS
      CHARACTER KEYW(NK)*8, TTITLE*56, TITLE(MAXANC)*24, DATE*11,
     *   UNITS(MAXANC)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH),  (HOLTMP, ITEMP)
      DATA NTT /56/
      DATA TTITLE /'AIPS UV DATA FILE ANTENNA TABLE '/
      DATA DTYP /83, 31, 1, 14, 14, 12, 12, 2, 13, 12, 2, 13, 12, 2/
      DATA TITLE /'ANNAME', 'STABXYZ', 'ORBPARM', 'NOSTA', 'MNTSTA',
     *   'STAXOF', 'DIAMETER', 'BEAMFWHM', 'POLTYA', 'POLAA', 'POLCALA',
     *   'POLTYB', 'POLAB', 'POLCALB'/
      DATA KEYW /'ARRAYX', 'ARRAYY', 'ARRAYZ', 'GSTIA0', 'DEGPDY',
     *   'FREQ', 'RDATE', 'POLARX', 'POLARY', 'UT1UTC', 'DATUTC',
     *   'TIMSYS', 'ARRNAM', 'XYZHAND', 'FRAME', 'NUMORB', 'NOPCAL',
C                                       Leave IATUTC last to look for
C                                       old tables.
     *   'NO_IF', 'FREQID', 'IATUTC'/
      DATA UNITS /' ', 'METERS', 3*' ', 'METERS', 'METERS', 'DEGR/M',
     *   ' ', 'DEGREES',' ', ' ', 'DEGREES', ' '/
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 30
      NCOL = MAXANC
      NDATA = MAXANC
      NKEY = NK
C                                       Fill in types, lengths
      IF (.NOT.DOREAD) THEN
         DO 10 I = 1,NDATA
            DATP(I,2) = DTYP(I)
 10         CONTINUE
         DATP(3,2) = DATP(3,2) + 10 * NUMORB
         DATP(8,2) = DATP(8,2) + 10 * ANTNIF
         DATP(11,2) = DATP(11,2) + 10 * NOPCAL * ANTNIF
         DATP(14,2) = DATP(14,2) + 10 * NOPCAL * ANTNIF
         END IF
C                                       Create/open file
      CALL TABINI (OPCODE, 'AN', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ANINI', IERR)
         GO TO 990
         END IF
      NEWFIL = (IERR.LT.0)
      NKEYRD = BUFFER(53)
C                                       Get number of records
      IANRNO = BUFFER(5) + 1
      IF (DOREAD) IANRNO = 1
      NKEY = NK
C                                       File created, initialize
      IF (NEWFIL) THEN
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ANINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, ITEMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'ANINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
C                                       Set keyword values
C                                       Write old 'IATUTC' for
C                                       compatibility.  This should be
C                                       removed after about 1993.
         IPOINT = 1
C                                       Array center
         KLOCS(1) = IPOINT
         KEYTYP(1) = 1
         CALL RCOPY (NWDPDP, ARRAYC(1), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(2) = IPOINT
         KEYTYP(2) = 1
         CALL RCOPY (NWDPDP, ARRAYC(2), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(3) = IPOINT
         KEYTYP(3) = 1
         CALL RCOPY (NWDPDP, ARRAYC(3), KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       GST at time=0
         KLOCS(4) = IPOINT
         KEYTYP(4) = 1
         CALL RCOPY (NWDPDP, GSTIA0, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Earth rotation rate
         KLOCS(5) = IPOINT
         KEYTYP(5) = 1
         CALL RCOPY (NWDPDP, DEGPDY, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Subarray frequency
         KLOCS(6) = IPOINT
         KEYTYP(6) = 1
         CALL RCOPY (NWDPDP, SAFREQ, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Reference date
         KLOCS(7) = IPOINT
         KEYTYP(7) = 3
         CALL CHR2H (8, RDATE, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       Polar x and y posn.
         XYPOL(1) = POLRXY(1)
         XYPOL(2) = POLRXY(2)
C                                       check for units being meters
         RADSQ = XYPOL(1)*XYPOL(1) + XYPOL(2)*XYPOL(2)
         IF (RADSQ.GT.0.36) THEN
            XYPOL(1) = XYPOL(1) / 30.922
            XYPOL(2) = XYPOL(2) / 30.922
            END IF
         KLOCS(8) = IPOINT
         KEYTYP(8) = 1
         KEYVAD = XYPOL(1)
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
         KLOCS(9) = IPOINT
         KEYTYP(9) = 1
         KEYVAD = XYPOL(2)
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       UT1 - UTC
         KLOCS(10) = IPOINT
         KEYTYP(10) = 1
         KEYVAD = UT1UTC
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Time - UTC
         KLOCS(11) = IPOINT
         KEYTYP(11) = 1
         KEYVAD = DATUTC
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Time system
         KLOCS(12) = IPOINT
         KEYTYP(12) = 3
         CALL CHR2H (8, TIMSYS, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       Array name
         KLOCS(13) = IPOINT
         KEYTYP(13) = 3
         CALL CHR2H (8, ANAME, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       XYZ handedness
         KLOCS(14) = IPOINT
         KEYTYP(14) = 3
         CALL CHR2H (8, XYZHAN, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       Terrestrial frame
         KLOCS(15) = IPOINT
         KEYTYP(15) = 3
         CALL CHR2H (8, TFRAME, 1, KEYVAH(IPOINT))
         IPOINT = IPOINT + 2
C                                       No. orbital parameters
         KLOCS(16) = IPOINT
         KEYTYP(16) = 4
         KEYVAL(IPOINT) = NUMORB
         IPOINT = IPOINT + 1
C                                       No. polarization cal. parms.
         KLOCS(17) = IPOINT
         KEYTYP(17) = 4
         KEYVAL(IPOINT) = NOPCAL
         IPOINT = IPOINT + 1
C                                       No. IFs
         KLOCS(18) = IPOINT
         KEYTYP(18) = 4
         KEYVAL(IPOINT) = ANTNIF
         IPOINT = IPOINT + 1
C                                       Freqid of AN values
         KLOCS(19) = IPOINT
         KEYTYP(19) = 4
         KEYVAL(IPOINT) = ANFQID
         IPOINT = IPOINT + 1
C                                       Real IAT-UTC
         CALL JULDAY (RDATE, JD)
         UTC = JD - 2400000.5
         CALL LPSEC (UTC, SECS, DATE)
         KLOCS(20) = IPOINT
         KEYTYP(20) = 1
         KEYVAD = SECS
         CALL RCOPY (NWDPDP, KEYVAD, KEYVAR(IPOINT))
         IPOINT = IPOINT + NWDPDP
C                                       Write if just created
         CALL TABKEY (OPCODE, KEYW, NKEY, BUFFER, KLOCS, KEYVAL,
     *      KEYTYP, IERR)
         NKEYRD = NKEY
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'ANINI', IERR)
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
            CALL TABERR ('READ', 'TABKEY', 'ANINI', IERR)
            GO TO 990
            END IF
C                                       Retrieve keyword values
C                                       Array center
         IPOINT = KLOCS(1)
         IF (KEYTYP(1).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(1))
         ELSE
            ARRAYC(1) = KEYVAR(IPOINT)
            END IF
         IPOINT = KLOCS(2)
         IF (KEYTYP(2).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(2))
         ELSE
            ARRAYC(2) = KEYVAR(IPOINT)
            END IF
         IPOINT = KLOCS(3)
         IF (KEYTYP(3).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), ARRAYC(3))
         ELSE
            ARRAYC(3) = KEYVAR(IPOINT)
            END IF
C                                       GST at IAT=0
         IPOINT = KLOCS(4)
         IF (KEYTYP(4).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), GSTIA0)
         ELSE
            GSTIA0 = KEYVAR(IPOINT)
            END IF
C                                       Earth rotation rate
         IPOINT = KLOCS(5)
         IF (KEYTYP(5).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), DEGPDY)
         ELSE
            DEGPDY = KEYVAR(IPOINT)
            END IF
C                                       Subarray frequency
         IPOINT = KLOCS(6)
         IF (KEYTYP(6).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), SAFREQ)
         ELSE
            SAFREQ = KEYVAR(IPOINT)
            END IF
C                                       Reference date
         IPOINT = KLOCS(7)
         CALL H2CHR (8, 1, KEYVAH(IPOINT), RDATE)
C                                       Polar x and y posn.
         IPOINT = KLOCS(8)
         IF (KEYTYP(8).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            XYPOL(1) = KEYVAD
         ELSE
            XYPOL(1) = KEYVAR(IPOINT)
            END IF
         IPOINT = KLOCS(9)
         IF (KEYTYP(9).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            XYPOL(2) = KEYVAD
         ELSE
            XYPOL(2) = KEYVAR(IPOINT)
            END IF
C                                       check for units being meters
         RADSQ = XYPOL(1)*XYPOL(1) + XYPOL(2)*XYPOL(2)
         IF (RADSQ.GT.0.36) THEN
            XYPOL(1) = XYPOL(1) / 30.922
            XYPOL(2) = XYPOL(2) / 30.922
            END IF
         POLRXY(1) = XYPOL(1)
         POLRXY(2) = XYPOL(2)
C                                       UT1 - UTC
         IPOINT = KLOCS(10)
         IF (KEYTYP(10).EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
            UT1UTC = KEYVAD
         ELSE
            UT1UTC = KEYVAR(IPOINT)
            END IF
C                                       Time system.
         IF (KLOCS(12).GT.0) THEN
            CALL H2CHR (8, 1, KEYVAH(KLOCS(12)), TIMSYS)
C                                       Defaults to IAT
         ELSE
            TIMSYS = 'IAT'
            END IF
C                                       Time - UTC
         IF (KLOCS(11).GT.0) THEN
C                                       has 'DATUTC'
            IPOINT = KLOCS(11)
            IF (KEYTYP(11).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
               DATUTC = KEYVAD
            ELSE
               DATUTC = KEYVAR(IPOINT)
               END IF
C                                       has 'IATUTC'
         ELSE IF ((KLOCS(20).GT.0) .AND. (TIMSYS.NE.'UTC')) THEN
            IPOINT = KLOCS(20)
            IF (KEYTYP(20).EQ.1) THEN
               CALL RCOPY (NWDPDP, KEYVAR(IPOINT), KEYVAD)
               DATUTC = KEYVAD
            ELSE
               DATUTC = KEYVAR(IPOINT)
               END IF
C                                       no time correction to UTC
         ELSE
            DATUTC = 0.0
            END IF
C                                       Array name
         CALL H2CHR (8, 1, KEYVAH(KLOCS(13)), ANAME)
C                                       handedness
         IF (KLOCS(14).GT.0) THEN
            CALL H2CHR (8, 1, KEYVAH(KLOCS(14)), XYZHAN)
C                                       Defaults to IAT
         ELSE
            XYZHAN = '?????'
            END IF
C                                       Terrestrial frame
         IF (KLOCS(15).GT.0) THEN
            CALL H2CHR (8, 1, KEYVAH(KLOCS(15)), TFRAME)
C                                       Defaults to IAT
         ELSE
            TFRAME = '?????'
            END IF
C                                       No. orbital parameters
         NUMORB = KEYVAL(KLOCS(16))
C                                       No. polarization cal. parms.
         NOPCAL = KEYVAL(KLOCS(17))
C                                       Number IFs
         IF (KLOCS(18).GT.0) THEN
            ANTNIF = KEYVAL(KLOCS(18))
         ELSE
            ANTNIF = 0
            END IF
C                                       ANFQID
         IPOINT = KLOCS(19)
         IF (IPOINT.GT.0) THEN
            ANFQID = KEYVAL(IPOINT)
         ELSE
            ANFQID = -999
            END IF
         END IF
      IERR = 0
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, ITEMP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'ANINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'AN', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'ANINI', IERR)
         GO TO 990
         END IF
C                                       Get array indices
      CALL FNDCOL (MAXANC, TITLE, 24, .TRUE., BUFFER, ANKOLS, JERR)
C                                       Get array indices and no. values
      DO 150 I = 1,MAXANC
         IPOINT = ANKOLS(I)
         IF (IPOINT.GT.0) THEN
            ANKOLS(I) = DATP(IPOINT,1)
            ANNUMV(I) = DATP(IPOINT,2) / 10
            IF (ANNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               IF ((I.NE.3) .AND. (I.NE.11) .AND. (I.NE.14))
     *            CALL MSGWRT (6)
               END IF
         ELSE
            ANKOLS(I) = -1
            ANNUMV(I) = 0
            NC = ITRIM (TITLE(I))
            WRITE (MSGTXT,1101) TITLE(I)(:NC)
            IF (TITLE(I).NE.'ORBPARM') CALL MSGWRT (6)
            END IF
 150     CONTINUE
      GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1990) IERR, OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ANINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('ANINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('ANINI: ERROR',I4,' INITIALIZING ANTENNA TABLE FOR ',A)
      END
