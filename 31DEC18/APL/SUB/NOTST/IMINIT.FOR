      SUBROUTINE IMINIT (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ROW, IMKOLS, IMNUMV, OBSCOD, RDATE, NUMSTK, STK1, NUMIF,
     *   NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY, CORREV, IERR)
C-----------------------------------------------------------------------
C! Open interferometer model table for reading or writing
C# EXT-appl VLBI Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2006, 2009, 2015
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
C   Open an interferometer model (IM) table for reading or writing.
C
C   If OPCODE is 'READ', VER is positive, and there is an IM table with
C   version number VER attached to the file with catalogue number CNO on
C   disk DISK that can be opened for reading then open the table for
C   reading, update BUFFER, IMKOLS, and IMNUMV, set ROW to 1, set
C   OBSCOD, RDATE, NUMSTK, STK1, NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX,
C   NUMPOL, NUMPLY, and CORREV from the table header, and set IERR to
C   zero.
C
C   If OPCODE is 'READ', VER is zero, and there is at least one IM table
C   attached to the file with catalogue number CNO on disk DISK that can
C   be opened for reading then open the highest numbered IM table for
C   reading, update BUFFER, IMKOLS, and IMNUMV, set ROW to 1, set VER to
C   the version number of the table that was opened, set OBSCOD, RDATE,
C   NUMSTK, STK1, NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY,
C   and CORREV from the table header, and set IERR to zero.
C
C   If OPCODE is 'WRIT', VER is positive, and there is an IM table with
C   version number VER attached to the file with catalogue number CNO on
C   disk DISK that can be opened for writing then open the table for
C   writing, update BUFFER, IMKOLS, and IMNUMV, set ROW to one more than
C   the number of rows in the table, set OBSCOD, RDATE, NUMSTK, STK1,
C   NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY, and CORREV
C   from the table header, and set IERR to zero.
C
C   If OPCODE is 'WRIT', VER is positive, there is no IM table with
C   version number VER attached to the file with catalogue number CNO on
C   disk DISK, and a new IM table can be attached to this file then
C   create a new IM table with version number VER and open the table for
C   writing, update BUFFER, IMKOLS, and IMNUMV, set ROW to one,
C   enter the values of OBSCOD, RDATE, NUMSTK, STK1, NUMIF, NUMCHN,
C   REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY, and CORREV in the table
C   header, and set IERR to zero.
C
C   If OPCODE is 'WRIT', VER is zero, and a new IM table can be attached
C   to the file with catalogue number CNO on disk DISK then create a new
C   IM table and open the table for writing, update BUFFER, IMKOLS, and
C   IMNUMV, set ROW to one, set VER to the version number of the new
C   table, enter the values of OBSCOD, RDATE, NUMSTK, STK1, NUMIF,
C   NUMCHN, REFFRQ, CHANBW, REFPIX, NUMPOL, NUMPLY, and CORREV in the
C   table header, and set IERR to zero.
C
C   Otherwise issue one or more error messages and set IERR to a
C   non-zero value.
C
C   Inputs:
C      OPCODE  C*4      'READ' or 'WRIT'
C      DISK    I        disk number
C      CNO     I        catalogue number
C      LUN     I        LUN for table I/O (must be valid for table I/O)
C
C   Input/Output:
C      VER     I        requested table version number or zero on entry,
C                       actual table version number on exit
C      CATBLK  I(256)   catalogue block for file CNO on DISK
C      OBSCOD  C*8      observing code
C      RDATE   C*8      reference date (YYYYMMDD)
C      NUMSTK  I        size of STOKES axis in parent data file
C      STK1    I        reference value for STOKES axis in parent data
C                       file
C      NUMIF   I        size of IF axis in parent data file
C      NUMCHN  I        size of FREQ axis in parent data file
C      REFFRQ  I        reference frequency in Hz
C      CHANBW  I        channel spacing in Hz
C      REFPIX  I        reference pixel number for FREQ axis
C      NUMPOL  I        number of polarizations in table (1 or 2)
C      NUMPLY  I        number of polynomial terms used in table
C      CORREV  I        revision number of correlator software that
C                       generated the original table
C
C   Output:
C      BUFFER  I(512)   I/O control block and data buffer for table
C      ROW     I        first row to read or write
C      IMKOLS  I(20)    index of first data word for each logical
C                       column in a table record
C      IMNUMV  I(20)    dimension of each logical column
C      IERR    I        return status: 0 if table opened, non-zero
C                                      otherwise
C
C   Note:
C      A similar routine, IMINI, uses common blocks instead of explicit
C      arguments. You should open the table with IMINI if you wish to
C      open the table for use with GETDEL.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OBSCOD*8, RDATE*8
      INTEGER   BUFFER(512), DISK, CNO, VER, CATBLK(256), LUN,
     *   ROW, IMKOLS(20), IMNUMV(20), NUMSTK, STK1, NUMIF, NUMCHN,
     *   NUMPOL, NUMPLY, IERR
      DOUBLE PRECISION REFFRQ, CHANBW, REFPIX, CORREV
C
C     Local variables
C
C     TTITLE            table title
C     CTITLE            column titles
C     CUNITS            column units
C     TABREV            current table revision
C     TREVN             existing table revision number
C     NKEY              number of keyword/values in table header
C     NCOL              number of columns in table
C     NREC              number of rows to add to the table when it
C                       is extended
C     DATP              column descriptors
C     COL               number of defined columns
C     HOLTMP            Hollerith buffer
C     KEYS              keywords
C     KTYPE             keyword value types
C     KVALUE            keyword value array
C     KLOC              indices of keyword values in KVALUE
C
      CHARACTER TTITLE*56
      PARAMETER (TTITLE = 'INTERFEROMETER MODEL TABLE')
      CHARACTER CTITLE(20)*24, CUNITS(20)*8
      INTEGER   TABREV
      PARAMETER (TABREV = 2)
      INTEGER   TREVN, NKEY, NCOL, NREC, DATP(128,2), COL, NDATA, NC,
     *   ITRIM, IPOINT, I, JERR
      HOLLERITH HOLTMP(6)
      CHARACTER KEYS(13)*8
      INTEGER   KTYPE(13), KVALUE(26), KLOC(13)
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CTITLE
     *   /'TIME                    ', 'TIME_INTERVAL           ',
     *    'SOURCE_ID               ', 'ANTENNA_NO              ',
     *    'ARRAY                   ', 'FREQID                  ',
     *    'I.FAR.ROT               ', 'FREQ.VAR                ',
     *    'PDELAY_1                ', 'GDELAY_1                ',
     *    'PRATE_1                 ', 'GRATE_1                 ',
     *    'DISP_1                  ', 'DDISP_1                 ',
     *    'PDELAY_2                ', 'GDELAY_2                ',
     *    'PRATE_2                 ', 'GRATE_2                 ',
     *    'DISP_2                  ', 'DDISP_2                 '/
      DATA CUNITS /'DAYS    ', 'DAYS    ', '        ', '        ',
     *             '        ', '        ', 'RAD/M**2', 'HZ      ',
     *             'SECONDS ', 'SECONDS ', 'HZ      ', 'SEC/SEC ',
     *             'SEC/M**2', 'S/S/M**2', 'SECONDS ', 'SECONDS ',
     *             'HZ      ', 'SEC/SEC ', 'SEC/M**2', 'S/S/M**2'/
      DATA KEYS /'RDATE   ', 'OBSCODE ', 'NO_STKD ', 'STK_1   ',
     *           'NO_BAND ', 'NO_CHAN ', 'REF_FREQ', 'CHAN_BW ',
     *           'REF_PIXL', 'NO_POL  ', 'NPOLY   ', 'REVISION',
     *           'TABREV  '/
C-----------------------------------------------------------------------
C
C     If OPCODE is 'READ' then initialize variables for a call to open
C     the table for reading and set IERR to zero. If OPCODE is 'WRIT'
C     then initialize variable for a call to open the table for writing
C     and set IERR to 0. Otherwise issue an erro message and set IERR to
C     a non-zero value.
C
      NREC = 30
      IERR =  0
      IF (OPCODE.EQ.'WRIT') THEN
         NKEY = 13
         NCOL = 8 + 6 * NUMPOL
         DATP(1,2)  = TABDBL + 10
         DATP(2,2)  = TABFLT + 10
         DATP(3,2)  = TABINT + 10
         DATP(4,2)  = TABINT + 10
         DATP(5,2)  = TABINT + 10
         DATP(6,2)  = TABINT + 10
         DATP(7,2)  = TABFLT + 10
         DATP(8,2)  = TABFLT + 10 * NUMIF
         DATP(9,2)  = TABDBL + 10 * NUMIF * NUMPLY
         DATP(10,2) = TABDBL + 10 * NUMPLY
         DATP(11,2) = TABDBL + 10 * NUMIF * NUMPLY
         DATP(12,2) = TABDBL + 10 * NUMPLY
         DATP(13,2) = TABFLT + 10
         DATP(14,2) = TABFLT + 10
         IF (NUMPOL.EQ.2) THEN
            DATP(15,2) = TABDBL + 10 * NUMIF * NUMPLY
            DATP(16,2) = TABDBL + 10 * NUMPLY
            DATP(17,2) = TABDBL + 10 * NUMIF * NUMPLY
            DATP(18,2) = TABDBL + 10 * NUMPLY
            DATP(19,2) = TABFLT + 10
            DATP(20,2) = TABFLT + 10
            END IF
      ELSE IF (OPCODE.EQ.'READ') THEN
         NKEY = 0
         NCOL = 0
      ELSE
         MSGTXT = 'IMINIT: INVALID OPCODE ''' // OPCODE // ''''
         CALL MSGWRT (7)
         IERR = 1
         GO TO 999
         END IF
C                                       open table
      CALL TABINI (OPCODE, 'IM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'IMINIT', IERR)
         GO TO 990
         END IF
C                                       new table
      IF (IERR.LT.0) THEN
         ROW = 1
C                                       Fill in the table title:
         CALL CHR2H (56, TTITLE, 1, BUFFER(101))
C                                       column labels, units
         DO 10 COL = 1,NCOL
            CALL CHR2H (24, CTITLE(COL), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, COL, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'IMINIT', IERR)
               GO TO 990
               END IF
            CALL CHR2H (8, CUNITS(COL), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, COL, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'IMINIT', IERR)
               GO TO 990
               END IF
 10         CONTINUE
C                                       Set keyword values:
         KLOC(1) = 1
         KTYPE(1) = TABHOL
         CALL CHR2H (8, RDATE, 1, KVALUE(KLOC(1)))
         KLOC(2) = KLOC(1) + 2
         KTYPE(2) = TABHOL
         CALL CHR2H (8, OBSCOD, 1, KVALUE(KLOC(2)))
         KLOC(3) = KLOC(2) + 2
         KTYPE(3) = TABINT
         KVALUE(KLOC(3)) = NUMSTK
         KLOC(4) = KLOC(3) + 1
         KTYPE(4) = TABINT
         KVALUE(KLOC(4)) = STK1
         KLOC(5) = KLOC(4) + 1
         KTYPE(5) = TABINT
         KVALUE(KLOC(5)) = NUMIF
         KLOC(6) = KLOC(5) + 1
         KTYPE(6) = TABINT
         KVALUE(KLOC(6)) = NUMCHN
         KLOC(7) = KLOC(6) + 1
         KTYPE(7) = TABDBL
         CALL COPY (NWDPDP, REFFRQ, KVALUE(KLOC(7)))
         KLOC(8) = KLOC(7) + NWDPDP
         KTYPE(8) = TABDBL
         CALL COPY (NWDPDP, CHANBW, KVALUE(KLOC(8)))
         KLOC(9) = KLOC(8) + NWDPDP
         KTYPE(9) = TABDBL
         CALL COPY (NWDPDP, REFPIX, KVALUE(KLOC(9)))
         KLOC(10) = KLOC(9) + NWDPDP
         KTYPE(10) = TABINT
         KVALUE(KLOC(10)) = NUMPOL
         KLOC(11) = KLOC(10) + 1
         KTYPE(11) = TABINT
         KVALUE(KLOC(11)) = NUMPLY
         KLOC(12) = KLOC(11) + 1
         KTYPE(12) = TABDBL
         CALL COPY (NWDPDP, CORREV, KVALUE(KLOC(12)))
         KLOC(13) = KLOC(12) + NWDPDP
         KTYPE(13) = TABINT
         KVALUE(KLOC(13)) = TABREV
         NKEY = 13
         CALL TABKEY ('WRIT', KEYS, NKEY, BUFFER, KLOC, KVALUE, KTYPE,
     *      IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('WRIT', 'TABKEY', 'IMINIT', IERR)
            GO TO 990
            END IF
C                                       Read an existing table
      ELSE
         NKEY = 1
         CALL TABKEY ('READ', KEYS, NKEY, BUFFER, KLOC, KVALUE, KTYPE,
     *      IERR)
         IF (IERR.EQ.0) THEN
            CALL H2CHR (8, 1, KVALUE(KLOC(1)), RDATE)
         ELSE
            RDATE = ' '
            END IF
         NKEY = 12
         CALL TABKEY ('READ', KEYS(2), NKEY, BUFFER, KLOC, KVALUE,
     *      KTYPE, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.20)) THEN
            CALL TABERR ('READ', 'TABKEY', 'IMINIT', IERR)
            GO TO 990
            END IF
C                                       Keyword values Unpack
         CALL H2CHR (8, 1, KVALUE(KLOC(1)), OBSCOD)
         NUMSTK = KVALUE(KLOC(2))
         STK1   = KVALUE(KLOC(3))
         NUMIF  = KVALUE(KLOC(4))
         NUMCHN = KVALUE(KLOC(5))
         CALL COPY (NWDPDP, KVALUE(KLOC(6)), REFFRQ)
         CALL COPY (NWDPDP, KVALUE(KLOC(7)), CHANBW)
         CALL COPY (NWDPDP, KVALUE(KLOC(8)), REFPIX)
         NUMPOL = KVALUE(KLOC(9))
         NUMPLY = KVALUE(KLOC(10))
         CALL COPY (NWDPDP, KVALUE(KLOC(11)), CORREV)
         TREVN  = KVALUE(KLOC(12))
         IF (TREVN.NE.TABREV) THEN
            WRITE (MSGTXT,1014) TREVN, TABREV
            CALL MSGWRT (7)
            IERR = 1
            END IF
         END IF
C                                       dimensions
      NDATA = 8 + 6 * NUMPOL
      IF (OPCODE.EQ.'READ') THEN
         ROW = 1
      ELSE
         ROW = BUFFER(5) + 1
         END IF
C                                      Get array indices
C                                      Cover your ass from FNDCOL -
C                                      close to flush the buffers and
C                                      then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, KVALUE, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'IMINIT', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'IM', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'IMINIT', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (NDATA, CTITLE, 24, .TRUE., BUFFER, IMKOLS, JERR)
C                                      Get array indices and no. values
      DO 150 I = 1,NDATA
         IPOINT = IMKOLS(I)
         IF (IPOINT.GT.0) THEN
            IMKOLS(I) = DATP(IPOINT,1)
            IMNUMV(I) = DATP(IPOINT,2) / 10
            IF (IMNUMV(I).LE.0) THEN
               NC = ITRIM (CTITLE(I))
               WRITE (MSGTXT,1100) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            IMKOLS(I) = -1
            IMNUMV(I) = 0
            NC = ITRIM (CTITLE(I))
            WRITE (MSGTXT,1101) CTITLE(I)(:NC)
            CALL MSGWRT (6)
            END IF
 150     CONTINUE
      IERR = 0
      GO TO 999
C                                      Error
 990  WRITE (MSGTXT,1990) OPCODE
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1014 FORMAT ('IMINIT: FORMAT REVISION IS ', I2, ' EXPECTED ', I2)
 1100 FORMAT ('IMINIT: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('IMINIT: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('IMINIT: ERROR INITIALIZING INTERFEROMETER MODEL TABLE',
     *   ' FOR ',A4)
      END
