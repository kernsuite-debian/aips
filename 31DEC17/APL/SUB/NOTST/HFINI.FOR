      SUBROUTINE HFINI (OPCODE, BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   IHFRNO, HFKOLS, HFNUMV, IERR)
C-----------------------------------------------------------------------
C! Creates/opens/initializes Haystack MkIII FRNGE (HF) table
C# EXT-appl VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2006
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
C   Creates and initializes MkIII Haystack FRNGE (fringe fitting) table.
C   Inputs:
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     BUFFER(512)  I   I/O buffer and related storage, also defines file
C                      if open.
C     DISK         I   Disk to use.
C     CNO          I   Catalog slot number
C     VER          I   HF file version
C     CATBLK(256)  I   Catalog header block.
C     LUN          I   Logical unit number to use
C   Output:
C     IHFRNO       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     HFKOLS(77)   I   The column pointer array in order,
C     HFNUMV(77)   I   Element count in each column.
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), DISK, CNO, VER, CATBLK(256), LUN, IHFRNO,
     *   HFKOLS(*), HFNUMV(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXHFC
C                                       MAXHFC = no. col. in table
      PARAMETER (MAXHFC = 77)
      HOLLERITH HOLTMP(6)
      CHARACTER TTITLE*56, TITLE1(17)*24, TITLE2(2)*24,
     *   TITLE3(5)*24, TITLE4(14)*24, TITLE5(12)*24, TITLE6(27)*24,
     *   TITLE(MAXHFC)*24, UNITS(MAXHFC)*8
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, NTT, DTYP(MAXHFC), NDATA,
     *   IPOINT, JERR, I, NC, ITRIM
      LOGICAL   T, DOREAD, NEWFIL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (TITLE(1), TITLE1), (TITLE(18), TITLE2),
     *   (TITLE(20), TITLE3), (TITLE(25), TITLE4), (TITLE(39), TITLE5),
     *   (TITLE(51), TITLE6)
      DATA T /.TRUE./
      DATA NTT /56/
      DATA TTITLE /'MkIII FRNGE Fringe fitting table '/
C                 1   2   3   4   5   6   7   8    9   10  11  12  13
      DATA DTYP /23, 64, 64, 44, 14, 14, 14, 14, 284, 564, 14, 64, 14,
C        14  15  16  17
     *   14, 14, 14, 14,
C         18   19
     *   284, 844,
C         20   21   22  23  24
     *   284, 564, 284, 14, 14,
C        25  26  27  28  29  30  31  32  33  34  35  36  37  38
     *   83, 83, 83, 63, 83, 83, 63, 83, 13, 23, 63, 63, 83, 83,
C         39  40  41  42  43  44  45  46  47  48  49  50
     *   141, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
C         51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
     *   282, 22, 12, 12, 12, 12, 12, 12, 22, 22, 12, 22, 62, 12, 12,
C        66  67  68  69  70  71  72  73  74  75  76  77
     *   12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12/
C                                       Type 4000 records
C                   1         2           3         4           5
      DATA TITLE1 /'XY ', 'UTC TAG', 'VLB2 UTC', 'ARCHIV', 'SAMPRATE',
C           6         7            8             9            10
     *   'FR/PP', 'PASS NO.', 'NO. CHANELS', 'NO. OF AP', 'RECTRACK',
C               11                 12           13
     *   'COREL VERSION NO.', 'UTCM TAG', 'LU OF PRINTOUT',
C                14                  15
     *   'REF. TAPE DRIVE NO.',  'REM. TAPE DRIVE NO.',
C                16                17
     *   'SPECIAL OPTIONS', 'INTEGER PARM(250)'/
C                                       Type 4100 records
C                      18           19
      DATA TITLE2 /'CORELXTNT', 'CLBYFRQ'/
C                                       Type 4200 records
C                      20         21         22            23
      DATA TITLE3 /'PROCUTC', 'ERRORATE', 'INDEX', 'FRNGE ERROR CODE',
C               24
     *   'SBDOFFST FLAG'/
C                                       Type 4300 records
C                      25            26              27
      DATA TITLE4 /'STAR ID', 'BASELINE ANT1', 'BASELINE ANT2',
C             28           29              30           31
     *   'CORELFILE', 'TAPEID ANT1', 'TAPEID ANT2', 'VLB2PRG',
C              32            33                    34
     *   'RUN CODE', 'FRNGE QUALITY CODE', 'FREQ. GROUP CODE',
C                  35                    36                37
     *   'ORIG. COREL FILE NAME', 'TAPE Q CODE', 'REF OCCUPATION CODE',
C                   38
     *   'REM OCCUPATION CODE'/
C                                       Type 4400 records
C                     39        40          41          42
      DATA TITLE5 /'RFREQ', 'REF FREQ', 'DEL OBSV' ,'RAT OBSV',
C            43         44      45      46          47
     *   'NB DELAY', 'DGPD', 'BTE0', 'EPOCH0', 'DEL OBSVM',
C             48        49      50
     *   'RAT OBSVM', 'DLY2', 'DLY3'/
C                                       Type 4500 records
C                      51        52           53           54
      DATA TITLE6 /'AMBYFRQ', 'PHASECAL', 'DELRESID', 'DELSIGMA',
C             55         56          57          58         59
     *   'RATRESID', 'RATSIGMA', 'COHERCOR', 'TOTPHASE', 'UVF/ASEC',
C             60        61      62          63        64        65
     *   'STARELEV', 'AAMP', 'URVRSEC', 'SRCHPAR', 'DEPSBRES', 'SNR',
C          66       67      68       69      70       71     72
     *   'PROB', 'INCOH', 'EARP', 'REARP', 'START', 'STOP', 'EPD',
C          73      74     75     76       77
     *   'DUR', 'DELSS', 'QB', 'DISCD', 'TOTPM'/
      DATA UNITS /17*'        ', 2*'        ', 5*'        ',
     *   14*'        ',
C          39     40      41        42        43     44      45
     *   'MHZ', 'MHZ', 'USEC', 'USEC/SEC', 'USEC', 'USEC', 'USEC',
C          46      47        48        49     50
     *   'MSEC', 'USEC', 'USEC/SEC', 'USEC', 'USEC',
C            51           52       53      54        55          56
     *   '        ', 'USEC/SEC', 'USEC', 'USEC', 'USEC/SEC', 'USEC/SEC',
C            57        58      59       60      61          62
     *   '        ', 'DEG', '1/ASEC', 'DEG', '        ', 'MHZ/SEC',
C           63         64       65-67        68     69      70     71
     *   '        ', 'USEC', 3*'        ', 'DEG', 'DEG',  'SEC', 'SEC',
C         72     73      74       75-76        77
     *   'SEC', 'SEC', 'USEC', 2*'        ', 'DEG'/
C
     *
C-----------------------------------------------------------------------
C                                       Check OPCODE
      DOREAD = OPCODE.EQ.'READ'
C                                       Open file
      NREC = 1000
      NCOL = MAXHFC
      IF (DOREAD) NCOL = 0
      NKEY = 0
      NDATA = MAXHFC
      CALL FILL (NDATA, 0, HFKOLS)
      CALL FILL (NDATA, 0, HFNUMV)
C                                       Fill in types
      IF (.NOT.DOREAD) CALL COPY (NDATA, DTYP, DATP(1,2))
C                                       Create/open file
      CALL TABINI (OPCODE, 'HF', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'HFINI', IERR)
         GO TO 990
         END IF
      NEWFIL = IERR.LT.0
C                                       Get number of scans
      IHFRNO = BUFFER(5) + 1
      IF (DOREAD) IHFRNO = 1
C                                       File created, initialize
      IF (NEWFIL) THEN
C                                       Col. labels.
         DO 40 I = 1,NCOL
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'HFINI', IERR)
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'HFINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (NTT, TTITLE, 1, BUFFER(101))
         END IF
C                                       Get array indices
C                                       Cover your ass from FNDCOL -
C                                       close to flush the buffers and
C                                       then reopen.
      CALL TABIO ('CLOS', 0, IPOINT, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('CLOS', 'TABIO', 'HFINI', IERR)
         GO TO 990
         END IF
      NKEY = 0
      CALL TABINI (OPCODE, 'HF', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR (OPCODE, 'TABINI', 'HFINI', IERR)
         GO TO 990
         END IF
      CALL FNDCOL (MAXHFC, TITLE, 24, T, BUFFER, HFKOLS, JERR)
C                                       column pointers, checks
      DO 150 I = 1,NDATA
         IPOINT = HFKOLS(I)
         IF (IPOINT.GT.0) THEN
            HFKOLS(I) = DATP(IPOINT,1)
            HFNUMV(I) = DATP(IPOINT,2) / 10
            IF (HFNUMV(I).LE.0) THEN
               NC = ITRIM (TITLE(I))
               WRITE (MSGTXT,1100) TITLE(I)(:NC)
               CALL MSGWRT (6)
               END IF
         ELSE
            HFKOLS(I) = -1
            HFNUMV(I) = 0
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
 1100 FORMAT ('HFINI: ''',A,''' COLUMN HAS NO VALUES')
 1101 FORMAT ('HFINI: ''',A,''' COLUMN NOT FOUND')
 1990 FORMAT ('HFINI: ERROR INITIALIZING HF TABLE FOR ',A4)
      END
