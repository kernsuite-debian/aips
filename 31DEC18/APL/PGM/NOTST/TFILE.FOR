C     Developed from AFILE.FOR    M. Wunderlich MPIfR Bonn  10-Mar-97
C     Last change:   MMW  07-may-97
C
LOCAL INCLUDE 'TFILE.INC'
      INTEGER   NUMLIN
      PARAMETER (NUMLIN=20000)
C
      HOLLERITH XINFIL(12), XOUTFL(12), XOPCOD
      CHARACTER INFIL*48, OUTFIL*48, OPCODE*4
      REAL      APARM(10)
      INTEGER   LUN1, LUN2, FIND1, FIND2, BUFF1(256), TOTLIN,
     *   EXPNUM, UVERSN
      CHARACTER LINE(NUMLIN)*512, TIT1*512, TIT2*512
      REAL      TIMIND(NUMLIN)
      INTEGER   BASIND(NUMLIN), AINDEX(NUMLIN)
      COMMON /INPARM/ XINFIL, XOUTFL, XOPCOD, APARM
      COMMON /AFILP/ LUN1, LUN2, FIND1, FIND2, TIMIND, BASIND, BUFF1,
     *   AINDEX, TOTLIN, EXPNUM, UVERSN
      COMMON /CHARPM/ INFIL, OUTFIL, OPCODE
      COMMON /CHDATA/ LINE, TIT1, TIT2
LOCAL END
      PROGRAM TFILE
C-----------------------------------------------------------------------
C! Task to sort and edit a MkIII correlator UNIX-based A-file.
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2012
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
C   TFILE allows a user to provide a subroutine which performs an
C   operation on a UV data base, writing an output UV data base.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFIL         Input text A-file (UNIX based)
C      OUTFILE        OUTFIL        Output text A-file (ditto)
C      APARM(10)      APARM         A-FILE selection criteria
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'TFILE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA PRGM /'TFILE '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
      CALL TFILIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TFILIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TFILIN gets input parameters for TFILE.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C
C
C
C  The documentation of the format is as follows:
C
C Unix-based A-file format              Documentor CJL, 28 January 1994
C -----------------------               -------------------------------
C
C Below is a description of the A-file format, version 2 and higher.
C Version 2 was introduced for two purposes.  First, deficiencies in the
C old format were fixed.  Various useful quantities were added, and the
C scan time precision was improved to 1 second, allowing trivial
C construction of a unique Unix data file name from the A-file line.
C Second, a mechanism for future support of A-file format evolution was
C included via the version number in field 1.
C
C A ground rule is that the new format is field-oriented, not column-
C oriented. Fields are delimited by spaces.  In general, A-file format
C writing routines will stick to the same columns to facilitate viewing
C of the files, but these columns are NOT fixed, and any software that
C uses these files will not care about the columns.  Many fields,
C however, will normally have fixed widths  (indicated by a definite
C number of characters in the width column below). Extreme values in any
C field cause the field width to expand.  Common to all lines is a
C format version number in the first field.  A non-numeric character
C implies version 1 (the old HP-1000 format).  Future modifications
C to the format will increment this version number.  All unix software
C will support all A-file format versions, including files with mixed
C formats.  In the latter case, however, certain fields will be missing
C in certain lines, and processing of those quantities may be blocked by
C programs that encounter such mixed-format files.
C
C Note that version 2+ or mixed format A-files CANNOT be used on the
C HP-1000 systems.  If you need to ship an A-file back there, for
C reprocessing perhaps, it MUST be a version 1 A-file.  Control over the
C output format from the unix programs is provided.
C
C Below are the formal definitions of the formats for different line
C types, format version number 4.
C
C The contents of the type 0 lines are as follows:
C
C Field # Contents              Width/type      Comments
C ------- --------              ----------      --------
C 1     Format version #        1/i             Non-numeric = version 1
C 2     Root id code            6/c             6-char lower case
C 3     File type (0)           1/i             Root file record
C 4     Extent #                2/i             Should always be zero
C 5     Size                    4/i             # of 256-byte records in
C                                                 file
C 6     Expt serial #           4/i             Part of filename
C 7     Procdate yyddd?hhmmss   12/c            Corel run date,
C                                                 ? is corel version
C 8     Year of scan            2/i             Two-digit year
C 9     Time tag ddd-hhmmss     10/c            Equals scan directory
C                                                 name
C 10    Source name             8/c             Blank padded
C 11    Station list            variable/c      1 character per station
C
C Changes version 1 to 2: Deletion of the archive number (obsolete
C                         under unix), repositioning of the root id code
C                         and addition of seconds precision to the scan.
C                         The FMGR file name is also dropped.
C Changes version 2 to 3: None
C Changes version 3 to 4: Added seconds precision to procdate
C
C
C The contents of the type 1 lines are as follows:
C
C Field # Contents              Width/type      Comments
C ------- --------              ----------      --------
C 1     Format version #        1/i             Non-numeric = version 1
C 2     Root id code            6/c             6-char lower case
C 3     File type (1)           1/i             Corel file record
C 4     Extent #                2/i             Part of filename
C 5     Size                    4/i             # of 256-byte records in
C                                                 file
C 6     Expt serial #           4/i             Part of filename
C 7     Procdate yyddd?hhmmss   12/c            Corel run date,
C                                                 ? is corel version
C 8     Year of scan            2/i             Two-digit year
C 9     Time tag ddd-hhmmss     10/c            Equals scan directory
C                                                 name
C 10    Source name             8/c             Blank-padded
C 11    Baseline/QF             3/c             2-char baseline id and
C                                                 1-char qcode
C                                               Meaning of qcode in
C                                                  corel.doc
C 12    Scheduled duration      4/i             Seconds
C 13    Lags                    4/i             Number of lags in
C                                                 correlation
C 14    Reference drive #       1/i
C 15    Remote drive #          1/i
C 16    frequencies             2/c             E.g. SX
C 17    Reference clock error   7/f             Microsec
C 18    Ref/rem clock diff.     7/f             Microsec
C 19    Status bits             6/octal         See corel.doc for
C                                               details
C
C Changes version 1 to 2: Repositioning of the root id code, addition of
C                         seconds precision to the scan, separating
C                         reference and remote drive numbers into two
C                         fields (in case they go over 9), omission of
C                         the scheduled scan start seconds (now in the
C                         scan field), and omission of correlation
C                         start/stop times and EQTs, which are not
C                         present in the root file, and therefore not
C                         accessible under UNIX.  The FMGR file name
C                         is also dropped.
C Changes version 2 to 3: None
C Changes version 3 to 4: Addition of field 13, and seconds precision
C                         to procdate
C
C
C The contents of the type 2 lines are as follows:
C
C Field #       Contents                Width/type      Comments
C -------       --------                ----------      --------
C 1     Format version #        1/i             Non-numeric = version 1
C 2     Root id code            6/c             6-char lower case
C 3     File type (2)           1/i             General format for
C                                               fringe data
C 4     Extent #                2/i             Part of filename
C 5     Duration                4/i             Nominal duration of this
C                                               datum (sec)
C 6     Length                  4/i             Seconds of data
C                                               represented.
C 7     Offset                  4/i             Offset (sec) of mean
C                                               time of data from
C                                               nominal scan time (field
C                                               11)
C 8     Expt serial #           4/i             Part of filename
C 9     Procdate yyddd?hhmmss   12/c            FRNGE/fourfit processing
C                                               time, char 6 is corel
C                                               version number
C 10    Year of scan            2/i             Two-digit year.
C 11    Time tag ddd-hhmmss     10/c            Nominal start time of
C                                               data record
C 12    Scan offset             3/i             Offset from scan time to
C                                               time tag (sec)
C 13    Source name             8/c             Blank-padded ascii
C                                               string
C 14    Baseline/QF             3/c             2-char baseline id and
C                                               1-char qcode; Meaning of
C                                               qcode defined in fourfit
C 15    Freq/mode/#freq         4/c             e.g. XC08 for X-band
C                                               mode C 8 freqs
C 16    Lags                    4/i             Number of lags in
C                                               correlation
C 17    Amplitude               6/f             In units of 1/10000
C 18    Ampl. SNR               5/f             4 significant digits
C 19    Phase(deg)              5/f             Can be of various type,
C                                               see field 21
C 20    Phase SNR               5/f             4 significant digits
C 21    Data type               2/c             First char specifies
C                                               data origin:
C                                               A = ap by ap data from
C                                               fringex
C                                               C = coherently avgd data
C                                                   from fringex
C                                               I = incoherently avgd
C                                                   data from average
C                                               J = Inc. scan avg from
C                                                   average, dur=seglen
C                                               K = Cofit output (bogus
C                                                   duration)
C                                               S = scan summary from
C                                                   alist
C                                               O = overlapping coh.
C                                                   avg. from fringex
C                                               Second char specifies
C                                               phase type:
C                                               f = residual to
C                                                   corel+fourfit, e.c.
C                                               c = residual to corel
C                                                   only
C                                               t = total phase
C                                                 (geodetic definition)
C                                               e = total earth-centered
C                                                   phase
C 22    Resid SBD               5/f             Microseconds
C 23    Resid MBD               8/f             Microseconds
C 24    MBD ambiguity           6/f             Microseconds
C 25    Resid rate              7/f             Picoseconds/second
C 26    Ref. elevation          4/f             At reference epoch,
C                                               degrees
C 27    Rem. elevation          4/f             At reference epoch,
C                                               degrees
C 28    Ref. azimuth            5/f             At reference epoch,
C                                               degrees
C 29    Rem. azimuth            5/f             At reference epoch,
C                                               degrees
C 30    u (megalambda)          6/f             precision 4 sig. digits
C 31    v (megalambda)          6/f             precision 4 sig. digits
C 32    ESDESP                  6/c             E=ref.tape error rate
C                                               exponent:
C                                                  0=1.0E00  9=1.0E-9
C                                               S=ref.tape slip sync
C                                               rate:
C                                                     0=10%     9=1%
C                                               D=percentage data
C                                               discarded:
C                                                     0=10%     9=1%
C                                               E=rem.tape error rate
C                                               exponent:
C                                                    0=1.0E00  9=1.0E-9
C                                               S=rem.tape slip sync
C                                               rate:
C                                                    0=10%     9=1%
C                                               P=fraction of data
C                                               processed: i.e.
C                                               INTG.TIME/scheduled run
C                                               duration
C 33    Reference epoch         4/c             mmss at which various
C                                               time-dependent
C                                               quantities are
C                                               calculated
C 34    Reference frequency     8/f             Precision 10 KHz
C 35    Total e.c. phase        5/f             Regardless of field 21
C 36    Total drate             11/f            At ref epoch,
C                                               microsec/sec
C 37    Total mbdelay           13/f            At ref epoch, microsec
C 38    Total SBD-MBD           5/f             At ref epoch, microsec
C 39    Search coh. time        3/i             Seconds
C 40    Zero-loss coh. time     3/i             Seconds
C 41    Parent corel extent(s)  variable        up to 4 numbers,
C                                                "a,b,c,d"
C
C
C Changes version 1 to 2:
C     Brand new fields are 1, 21, 26, 27, 28, 29, 30, 31, 34.
C     Modified or enhanced fields are 11, 19, 22, 23, 41.
C     Discarded fields are FMGR file name, reference and remote tape
C        labels, A-file number, phasecal numbers for first and last
C        channels, reference and remote.
C Changes version 2 to 3: Addition of fields 5 and 7.
C Changes version 3 to 4: Addition of fields 12, 16, 20, 39 and 40, and
C     seconds precision to procdate
C
C The above is intended to fulfill data export needs, but is
C insufficient for astrometric purposes.
C
C
C The contents of the type 3 lines are as follows:
C
C Field # Contents              Width/type      Comments
C 1     Format version #        1/i
C 2     Expt serial #           4/i
C 3     File type (3)           1/i             Closure triangle data
C 4     Year of scan            2/i             Two-digit year.
C 5     Time tag ddd-hhmmss     10/c            Nominal start time of data record
C 6     Scan offset             3/i             Offset from scan time to time tag (sec)
C 7     Source name             8/c             Blank-padded ascii string
C 8     Freq/mode               2/c             e.g. XC for X-band mode C
C 9     Lags                    4/i             Number of lags in correlation
C 10    Triangle                3/c             e.g. ABC
C 11    root extension(s)       6-20/c          If more than one, comma separated
C 12    extents                 i,i,i           For baselines AB, BC, CA
C 13    lengths                 i,i,i           For baselines AB, BC, CA
C 14    Duration                4/i             Nominal duration of datum
C 15    Offset                  4/i             Offset from nominal scan time of
C                                               mean time of all contributing data
C 16    Scan quality            1/c             Based on baseline qf's
C 17    Data quality            1/c             Based on data values
C 18    ESDESP                  6/c             Minimum of baseline values
C 19    bispectral amp          9/f             Units of 10**-12
C 20    bispectral snr          7/f
C 21    bispectral phase        5/f             Closure phase (degrees)
C 22    Data type               2/c             Analogous to type 2 field 21,
C                                               meaning yet to be defined
C 23    closure sbd             6/f             Microseconds
C 24    closure mbd             8/f             Microseconds
C 25    closure mbd ambig.      6/f             Microseconds
C 26    closure delay rate      7/f             Picosec/sec
C 27    Elevations              f,f,f           For stations A,B,C
C 28    Azimuths                f,f,f           For stations A,B,C
C 29    Reference epoch         4/c             mmss at which various time-dependent
C                                               quantities are calculated
C 30    Reference frequency     8/f             Precision 10 KHz
C
C
C Changes version 1 to 2: There was no version 1.
C Changes version 2 to 3: Added fields 14, 15, 22.
C Changes version 3 to 4: Added fields 6, 9.
C
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INTEGER   IERR, NPARM, ICHAR, JTRIM, I, ILINE, IROUND
      INTEGER   PI(100)
      REAL      TIM1, TIM2
      LOGICAL   T, APPEND
      INCLUDE 'TFILE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 35
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (48, 1, XINFIL, INFIL)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IF ((OPCODE.NE.'SORT') .AND. (OPCODE.NE.'EDIT')) THEN
         WRITE (MSGTXT,1020) OPCODE
         JERR = 1
         GO TO 990
         END IF
      EXPNUM = IROUND(APARM(2))
      IF (EXPNUM.LE.0) EXPNUM = 0
C                                       Open text files
      LUN1 = 10
      LUN2 = 11
      APPEND = .FALSE.
      CALL ZTXOPN ('READ', LUN1, FIND1, INFIL, APPEND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, INFIL
         GO TO 990
         END IF
      CALL ZTXOPN ('WRIT', LUN2, FIND2, OUTFIL, APPEND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, OUTFIL
         GO TO 990
         END IF
C                                       Sort the data
      CALL DSORT (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Edit the data
      IF (OPCODE.EQ.'EDIT') THEN
         CALL DEDIT (JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
C                                       Write the final file.
      CALL ZTXIO ('WRIT', LUN2, FIND2, TIT1(:JTRIM(TIT1)), IERR)
      CALL ZTXIO ('WRIT', LUN2, FIND2, TIT2(:JTRIM(TIT2)), IERR)
      ILINE = 0
      TIM1 = 0.0
      TIM2 = 0.0
      DO 800 I = 1, TOTLIN
         IF ((OPCODE.EQ.'EDIT') .AND. (AINDEX(I).EQ.0)) GO TO 800
         ICHAR = JTRIM(LINE(I))
         CALL ZTXIO ('WRIT', LUN2, FIND2, LINE(I)(:ICHAR), IERR)
         ILINE = ILINE + 1
         IF (ILINE.EQ.1) THEN
            CALL UINDEX (LINE(I),PI)
            CALL TIMCRK (LINE(I)(PI(11):), TIM1)
            END IF
         IF (ILINE.GT.1) THEN
            CALL UINDEX (LINE(I),PI)
            CALL TIMCRK (LINE(I)(PI(11):), TIM2)
            IF (TIM2.LT.TIM1) THEN
               WRITE (MSGTXT,1040) ILINE
               GO TO 990
               END IF
            TIM1 = TIM2
            END IF
 800     CONTINUE
      WRITE (MSGTXT,1030) ILINE
      CALL MSGWRT (6)
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  CALL ZTXCLS (LUN1, FIND1, IERR)
      CALL ZTXCLS (LUN2, FIND2, IERR)
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AFILIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('AFILIN: ERROR',I3,' OPENING ',A)
 1020 FORMAT ('AFILIN: UNKNOWN OPCODE ',A4,' --- TRY AGAIN')
 1030 FORMAT ('Total number of type 52 lines in output A file = ',I7)
 1040 FORMAT ('AFILIN: line ',I6,' in output file out of time order')
      END
      SUBROUTINE DSORT (IERR)
C-----------------------------------------------------------------------
C  Routine to sort the data by time and baseline. will reject all
C  non type 52 records. It appears that type 52 summaries are the ones
C  we need for our purposes. So routine will first read up to 5000
C  type 52 records into memory, then set up a sorting index based
C  on time and baseline, then use the sorting index to write out the
C  records in the correct order.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'TFILE.INC'
      CHARACTER TEMP*512, TLIN(NUMLIN)*512
      INTEGER   ILINE, KLINE, XBASE(NUMLIN), I, J, K, II, JT, JTRIM,
     *   INDX(NUMLIN), ITIME, NTIM(NUMLIN), NSTART(NUMLIN), NL, OLDI,
     *   IT1, IGRP, NSTOP, PI(100), FTYPE
      REAL      XTIME(NUMLIN), T1
      LOGICAL   EOF, SORT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Read a line and decode the
C                                        time into days
      NL = NUMLIN
      CALL FILL (NL, 0, NTIM)
      CALL FILL (NL, 0, NSTART)
      ILINE = 0
      KLINE = 0
      IERR = 0
      EOF = .FALSE.
      TIT1 = ' '
      TIT2 = ' '
 100  CALL ZTXIO ('READ', LUN1, FIND1, TEMP, IERR)
      IF (IERR.EQ.2) THEN
         EOF = .TRUE.
         IERR = 0
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IF (EOF) GO TO 200
      JT = JTRIM (TEMP)
      KLINE = KLINE + 1
C                                        Parse line to get pointers
C                                        for data items
      CALL UINDEX (TEMP,PI)
C                                        Ignore comment lines
      IF (TEMP(1:1).EQ.' ') GO TO 100
      IF (TEMP(1:1).EQ.'*') GO TO 100
C                                        Check for old HP-1000 ver.
      IF (TEMP(1:1).EQ.'<') THEN
         WRITE (MSGTXT,1030)
         GO TO 990
         END IF
C                                        Field #1 is version no.
C                                        We do Ver. 4 only in the moment
C                                        In the future one can handle
C                                        several versions via UVERSN
      READ (TEMP(PI(1):),2000) UVERSN
2000  FORMAT (I2)
C
      IF (UVERSN.NE.4) THEN
         WRITE (MSGTXT,1040) UVERSN
         GO TO 990
         END IF
C                                        Field #3 is file type
      READ (TEMP(PI(3):),2010) FTYPE
2010  FORMAT (I2)
C                                        Is it a type 52 ?

C      IF (TEMP(8:8).NE.'2') GO TO 100
      IF (FTYPE.NE.2) GO TO 100
C                                        OK we want this one.
      ILINE = ILINE + 1
      LINE(ILINE) = TEMP
C                                        Field #11 is time tag
C                                        Extract time and encode as
C                                        days
      CALL TIMCRK (TEMP(PI(11):), TIMIND(ILINE))
C                                        Field #14 is baseline + qf
C                                        Extract baseline names and
C                                        encode as A + 256 * B
      CALL BASCRK (TEMP(PI(14):), BASIND(ILINE))
      GO TO 100
C                                        Now we sort by time
 200  DO 210 I = 1, ILINE
         XTIME(I) = TIMIND(I)
         XBASE(I) = BASIND(I)
         INDX(I) = I
         TLIN(I) = LINE(I)
 210     CONTINUE
C
      SORT = .FALSE.
      K = ILINE - 1
      DO 300 II = 1, ILINE
         DO 250 I = 1, K
            IF (TIMIND(I+1).LT.TIMIND(I)) THEN
               J = INDX(I)
               INDX(I) = INDX(I+1)
               INDX(I+1) = J
               T1 = TIMIND(I)
               TIMIND(I) = TIMIND(I+1)
               TIMIND(I+1) = T1
               END IF
 250        CONTINUE
 300     CONTINUE
C                                       reorder in memory
      ITIME = 0
      OLDI = 1
      TOTLIN = ILINE
 400  DO 450 I = 1, ILINE
         TIMIND(I) = XTIME(INDX(I))
         BASIND(I) = XBASE(INDX(I))
         LINE(I) = TLIN(INDX(I))
         IF (I.GT.1) THEN
            IF ((TIMIND(I).GT.TIMIND(I-1)).OR.(I.EQ.ILINE)) THEN
               ITIME = ITIME+ 1
               NTIM(ITIME) = I - OLDI
               IF (I.EQ.ILINE) NTIM(ITIME) = NTIM(ITIME) + 1
               NSTART(ITIME) = OLDI
               OLDI = I
               END IF
            END IF
 450     CONTINUE
C                                       Now sort by baseline order,
C                                       but must first gather groups
C                                       with the same time.
      DO 700 IGRP = 1, ITIME
         ILINE = NTIM(IGRP)
         K = ILINE + NSTART(IGRP) - 2
         NSTOP = NSTART(IGRP) + ILINE - 1
         DO 500 I = NSTART(IGRP), NSTOP
            XTIME(I) = TIMIND(I)
            XBASE(I) = BASIND(I)
            INDX(I) = I
            TLIN(I) = LINE(I)
 500        CONTINUE
C
         DO 550 II = NSTART(IGRP), NSTOP
            DO 520 I = NSTART(IGRP), K
               IF (BASIND(I+1).LT.BASIND(I)) THEN
                  J = INDX(I)
                  INDX(I) = INDX(I+1)
                  INDX(I+1) = J
                  IT1 = BASIND(I)
                  BASIND(I) = BASIND(I+1)
                  BASIND(I+1) = IT1
                  END IF
 520              CONTINUE
 550        CONTINUE
C                                       reorder in memory
 600     DO 610 I = NSTART(IGRP), NSTOP
            TIMIND(I) = XTIME(INDX(I))
            BASIND(I) = XBASE(INDX(I))
            LINE(I) = TLIN(INDX(I))
 610        CONTINUE
 700     CONTINUE
      WRITE (MSGTXT,1010) KLINE
      CALL MSGWRT (6)
      WRITE (MSGTXT,1020) TOTLIN
      CALL MSGWRT (6)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSORT: ERROR ',I3,' READING INPUT A-FILE')
 1010 FORMAT ('Total number of lines in input A file = ',I7)
 1020 FORMAT ('Total number of type-52 lines in input A file = ',I7)
 1030 FORMAT ('DSORT: OLD HP-RTE FORMATS NOT SUPPORTED.')
 1040 FORMAT ('DSORT: VERSION ',I2,' CURRENTLY NOR SUPPORTED.')
      END
      SUBROUTINE DEDIT (IERR)
C-----------------------------------------------------------------------
C  Routine to delete duplicate scans from the MkIII A-file. Upon
C  entering DEDIT the A-file will exist in memory, sorted by time
C  and baseline - so duplicate scans should lie together. This
C  routine will read all duplicate scans and delete those that are
C  unnecessary based on 4 criteria:
C    (1) take the one with the latest processing date
C    (2) take the one with the highest quality factor
C    (3) take the one with the highest signal-to-noise ratio
C    (4) take the one with the most data.
C  Also if EXPNUM > 0 will select scans based on experiment number
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'TFILE.INC'
      CHARACTER TLIN(NUMLIN)*512, QCODE*1, EXPCOD*4,
     *   ALPNUM*36
      INTEGER   ILINE, KLINE, BASE, B1, I, FLINE, QINDEX(500),
     *   NLINE, NL, K, LACC, CORCOD(500), CMAX, YR(500),
     *   DLEN(500), NUMEXP, YMAX,
     *   PI(100)
      REAL      TIME, T1
      REAL  SNR(500), SNRMAX
      DOUBLE PRECISION PTIME(500), TMAX
      LOGICAL   EOF
      INCLUDE 'INCS:DMSG.INC'
      DATA ALPNUM /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                        Message to user
      IF (EXPNUM.GT.0) THEN
         WRITE (MSGTXT,1140) EXPNUM
         CALL MSGWRT (6)
         END IF
      IF (APARM(1).EQ.1.0) WRITE (MSGTXT,1100)
      IF (APARM(1).EQ.2.0) WRITE (MSGTXT,1110)
      IF (APARM(1).EQ.3.0) WRITE (MSGTXT,1120)
      IF (APARM(1).EQ.4.0) WRITE (MSGTXT,1130)
      CALL MSGWRT (6)
C                                        Read until time and/or
C                                        baseline changes.
      ILINE = 0
      KLINE = 0
      IERR = 0
      NL = NUMLIN
      CALL FILL (NL, 1, AINDEX)
      EOF = .FALSE.
      TIME = -1.0
      BASE = -1
C
 100  CONTINUE
      ILINE = ILINE + 1
      KLINE = KLINE + 1
      IF (ILINE.GT.TOTLIN) GO TO 200
      TLIN(ILINE) = LINE(ILINE)
C
      CALL UINDEX (TLIN(ILINE), PI)
C
      IF (KLINE.EQ.1) THEN
         FLINE = ILINE
         CALL TIMCRK (TLIN(ILINE)(PI(11):), TIME)
         CALL BASCRK (TLIN(ILINE)(PI(14):), BASE)
         END IF
      CALL TIMCRK (TLIN(ILINE)(PI(11):), T1)
      CALL BASCRK (TLIN(ILINE)(PI(14):), B1)
      IF ((T1.GT.TIME) .OR. (B1.NE.BASE)) GO TO 200
C                                        Special case for last line
      IF (ILINE.EQ.TOTLIN) THEN
C                                        Check experiment number
         IF (EXPNUM.GT.0) THEN
            EXPCOD = TLIN(ILINE)(PI(8):)
            READ (EXPCOD,2000) NUMEXP
            IF (NUMEXP.NE.EXPNUM) AINDEX(ILINE) = 0
            END IF
         END IF
      GO TO 100
C                                        Do the editing.
 200  NLINE = KLINE - 1 + FLINE - 1
      KLINE = 0
      ILINE = ILINE - 1
C                                        Check for processing date,
C                                        choose latest
      IF (APARM(1).EQ.1.0) THEN
         K = 0
         LACC = FLINE
         TMAX = -1.0D10
         YMAX = -1
         CMAX = -1
         DO 250 I = FLINE, NLINE
            K = K + 1
            CALL UINDEX (TLIN(I), PI)
            CALL PROCDT (TLIN(I)(PI(9):), YR(K), PTIME(K), CORCOD(K))
            AINDEX(I) = 0
            IF (YR(K).EQ.YMAX) THEN
               IF (PTIME(K).GT.TMAX) THEN
                  TMAX = PTIME(K)
                  LACC = I
                  END IF
               END IF
C
            IF (YR(K).GT.YMAX) THEN
               YMAX = YR(K)
               TMAX = PTIME(K)
               LACC = I
               END IF
C
 250        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for quality factor,
C                                        choose highest
      IF (APARM(1).EQ.2.0) THEN
         K = 0
         LACC = FLINE
         YMAX = -1
         DO 300 I = FLINE, NLINE
            K = K + 1
            CALL UINDEX (TLIN(I), PI)
            QCODE = TLIN(I)((PI(14)+2):(PI(14)+2))
            QINDEX(K) = INDEX (ALPNUM,QCODE)
            IF (QINDEX(K).GT.10) QINDEX(K) = 1
            AINDEX(I) = 0
            IF (QINDEX(K).GT.YMAX) THEN
               YMAX = QINDEX(K)
               LACC = I
               END IF
 300        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for SNR
C                                        choose highest
      IF (APARM(1).EQ.3.0) THEN
         K = 0
         LACC = FLINE
         SNRMAX = 0.
         DO 350 I = FLINE, NLINE
            K = K + 1
            CALL UINDEX (TLIN(I), PI)
            CALL GETSNR (TLIN(I)(PI(18):), SNR(K))
            AINDEX(I) = 0
            IF (SNR(K).GT.SNRMAX) THEN
               SNRMAX = SNR(K)
               LACC = I
               END IF
 350        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for data length,
C                                        choose highest
      IF (APARM(1).EQ.4.0) THEN
         K = 0
         LACC = FLINE
         YMAX = -1
         DO 400 I = FLINE, NLINE
            K = K + 1
            CALL UINDEX (TLIN(I), PI)
            CALL GETLEN (TLIN(I)(PI(6):), DLEN(K))
            AINDEX(I) = 0
            IF (DLEN(K).GT.YMAX) THEN
               YMAX = DLEN(K)
               LACC = I
               END IF
 400        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check experiment number
      IF (EXPNUM.GT.0) THEN
         DO 710 I = FLINE, NLINE
            CALL UINDEX (TLIN(I), PI)
            EXPCOD = TLIN(I)(PI(8):)
            READ (EXPCOD,2010) NUMEXP
            IF (NUMEXP.NE.EXPNUM) AINDEX(I) = 0
 710        CONTINUE
         END IF
C                                       don't go back if last line has
C                                       been encountered
C
      IF (ILINE.LT.TOTLIN) GO TO 100
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DEDIT: ERROR ',I3,' READING INPUT A-FILE')
 1100 FORMAT ('Editing A-file based on last processing date')
 1110 FORMAT ('Editing A-file based on Quality Factor from FRNGE')
 1120 FORMAT ('Editing A-file based on Signal-to-Noise ratio')
 1130 FORMAT ('Editing A-file based on # seconds of data in scan')
 1140 FORMAT ('Selecting scans from experiment # ',I6)
 2000 FORMAT (I4)
 2010 FORMAT (I4)
      END
      SUBROUTINE TIMCRK (STRING, DAYS)
C-----------------------------------------------------------------------
C   Routine to crack the time from a character string
C   Input:
C     STRING       C*(*)       String containing time
C   Output:
C     DAYS         R           Time in days
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CTEMP*20
      REAL      DAYS, D, H, M, S
C-----------------------------------------------------------------------
      CTEMP = ' '
      CTEMP(1:3) = STRING(1:3)
      CTEMP(5:6) = STRING(5:6)
      CTEMP(8:9) = STRING(7:8)
      CTEMP(11:12) = STRING(9:10)
      READ (CTEMP,1000) D, H, M, S
1000  FORMAT (I3,I2,I2,I2)
      DAYS = D + (H/24.) + (M/(24.*60.)) + (S/(24.*60.*60.))
      RETURN
      END
      SUBROUTINE BASCRK (STRING, BASLN)
C-----------------------------------------------------------------------
C   Routine to crack the baseline from a character string, codify the
C   letters as numbers. BASLN = A*256 + B
C   Input:
C     STRING       C*(*)       String containing baseline
C   Output:
C     BASLN        I        Baseline number
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), A*1, B*1, ALPHA*26
      INTEGER   BASLN, I, J
      DATA ALPHA /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      A = STRING(1:1)
      B = STRING(2:2)
      I = INDEX(ALPHA,A)
      J = INDEX(ALPHA,B)
      BASLN = J + 256*I
      RETURN
      END
      SUBROUTINE PROCDT (STRING, YR, TIME, CORCOD)
C-----------------------------------------------------------------------
C   Routine to determine the processing date and COREL version.
C   Input:
C     STRING       C*(*)       String.
C   Output:
C     YR           I           Year of correlation
C     TIME         D           Time of processing (years)
C     CORCOD       I           Corel version (number = letter)
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), ALPHA*26
      REAL      D, H, M
      INTEGER   CORCOD, YR
      DOUBLE PRECISION TIME
      DATA ALPHA /'abcdefghijklmnopqrstuvwxyz'/
C-----------------------------------------------------------------------
      READ (STRING(1:2), '(I2)') YR
      READ (STRING(3:5), '(I3)') D
      IF (STRING(6:6).EQ.' ') CORCOD = 0
      IF (STRING(6:6).NE.' ') CORCOD = INDEX (ALPHA,STRING (6:6))
      READ (STRING(7:8), '(I2)') H
      READ (STRING(9:10), '(I2)') M
      TIME = D + (H/24.) + (M/(24.*60.))
      END
      SUBROUTINE GETSNR (STRING, SNR)
C-----------------------------------------------------------------------
C   Routine to extract the SNR.
C   Input:
C     STRING       C*(*)       String.
C   Output:
C     SNR          I           SNR
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CTEMP*4
      REAL      SNR
C-----------------------------------------------------------------------
      CTEMP = STRING(1:4)
      READ (CTEMP,1000) SNR
1000  FORMAT (F10.5)
      RETURN
      END
      SUBROUTINE GETLEN (STRING, LEN)
C-----------------------------------------------------------------------
C   Routine to extract the LEN.
C   Input:
C     STRING       C*(*)       String.
C   Output:
C     LEN          I           LEN
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CTEMP*4
      INTEGER   LEN
C-----------------------------------------------------------------------
      CTEMP = STRING(1:4)
      READ (CTEMP,1000) LEN
1000  FORMAT (I4)
      RETURN
      END
      SUBROUTINE UINDEX (LINE,PI)
C--------------------------------------------------------------------------
C     Utility to parse a line of text containing data fields.
C     Returns pointers to the data items (strings).
C     We assume that the first character in line is non-blank !
C
C     Input:  CHARACTER   LINE*512   A single line of text
C     Output: INTEGER     PI(*)      Pointer to data field
C--------------------------------------------------------------------------
      CHARACTER LINE*512, CTEMP
      INTEGER   PI(*)
      INTEGER   MAXL, I, PTR1, PTR2
C--------------------------------------------------------------------------
C                                      Initialize
      MAXL = 512
      PTR1 = 1
      I    = 0
C                                      Next data item
 50   I = I + 1
      PTR2 = 0
      PI(I) = PTR1
C                                      Read a single char from line
100   CTEMP = LINE (PTR1:)
      PTR1 = PTR1 + 1
      PTR2 = PTR2 + 1
C                                      Blank means end of string
      IF (CTEMP.NE.' ') GO TO 100
C                                      End of line ?
200   IF (PTR1.GE.MAXL) GO TO 300
C                                      There could be contiguos blanks
C                                      between data fields
      CTEMP = LINE (PTR1:)
      PTR1 = PTR1 + 1
C                                      Non-blank means next data item
      IF (CTEMP.EQ.' ') GO TO 200
C                                      Adjust pointer to next data
      PTR1 = PTR1 - 1
      GO TO 50
C                                      Found last data item
300   RETURN
      END
