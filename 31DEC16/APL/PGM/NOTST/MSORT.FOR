LOCAL INCLUDE 'MSORT.INC'
      INTEGER   CATBLK(256), SEQIN, SEQOUT, DISKIN, DISOUT, SORT1,
     *   SORT2, NSORT, IN, OUT, PRTLV
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XSORT
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, SORT*4
      REAL      XSIN, XSOUT, XDISIN, XDISO, XPRTLV
      LOGICAL   INPLAC
      COMMON /BUFRS/ IN, OUT
      COMMON /UVINF/ SEQIN, SEQOUT, DISKIN, DISOUT, SORT1,
     *   SORT2, NSORT, INPLAC, PRTLV
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XSORT, XPRTLV
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, SORT
LOCAL END
      PROGRAM MSORT
C-----------------------------------------------------------------------
C! Sorts uv data into a specified order.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2003, 2005, 2008, 2012, 2015
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
C   MSORT sorts a uv data base into a specified order
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of the file to be sorted
C   INCLASS        CLASIN        Class of the file to be sorted.
C   INSEQ          SEQIN         Seq. number of file to be sorted.
C   INDISK         DISKIN        Disk number of file to be sorted.
C   OUTNAME        NAMOUT        Name of the output sorted file.
C   OUTCLASS       CLAOUT        Class of the output sorted file.
C   OUTSEQ         SEQOUT        Seq. number of output sorted file.
C   OUTDISK        DISKO         Disk number of the output file.
C   SORT           SORT          Two char. sort order, eg. 'XY'
C                                Second key varies faster
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, BUFFER(256)
      INCLUDE 'MSORT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'MSORT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 18
      CALL MSRTIN (PRGM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read and partially sort data
C                                       to scratch file.
      IRET = 17
      CALL INMSRT (IERR)
      IF (IERR.NE.0) GO TO 990
      IRET = 0
C                                       Close down files
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE MSRTIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   MSRTIN gets input parameters for MSORT and creates an output file
C   if necessary.
C   Inputs: PRGM   C*6       Task name
C   Output: JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, HILINE*72, XSORTC(11)*1, TYPES(6)*4, STAT*4,
     *   XSORT1*1, XSORT2*1, NOTAB(1)*2, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   JERR, NOPT, LUN1, LUN2, OLDCNO, NONOT, IROUND, NPARM,
     *   IERR, I, NTRY, IOFF, JOFF, BUFFER(512)
      LOGICAL   T
      REAL      CATR(256)
      INCLUDE 'MSORT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA NONOT, NOTAB /0,'  '/
      DATA NTRY /6/
      DATA TYPES /'DEC ','DEC-','MM  ','GLON','ELON','Y   '/
      DATA XSORTC /'B','T','U','V','W','R','P','X','Y','Z','M'/
      DATA LUN1, LUN2, NOPT /27,28, 11/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init IO characteristics
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 16
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         JERR = 8
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISOUT = IROUND (XDISO)
      PRTLV = IROUND (XPRTLV)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XSORT, SORT)
C                                        Default sort order is 'TB'
      IF (SORT.EQ.'    ') SORT = 'TB  '
C                                       Decode sort order.
      XSORT1 = SORT(1:1)
      XSORT2 = SORT(2:2)
      SORT1 = 8
      SORT2 = 9
      DO 10 I = 1,NOPT
         IF (XSORT1.EQ.XSORTC(I)) SORT1 = I
         IF (XSORT2.EQ.XSORTC(I)) SORT2 = I
 10      CONTINUE
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      NCFILE = 1
      FVOL(1) = DISKIN
      FCNO(1) = OLDCNO
      FRW(1) = 0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, SORT, 1, CATH(KITYP))
      CATBLK(KIIMS) = SEQOUT
      IOFF = 3
      DO 30 I = 1,NTRY
         CALL AXEFND (4, TYPES(I), KICTPN, CATH(KHCTP), JOFF, IERR)
         IF (IERR.EQ.0) IOFF = JOFF
         IERR = 0
 30      CONTINUE
C                                       Create output file.
      CCNO = 1
      FRW(2) = 3
      INPLAC = .FALSE.
      CALL UVCREA (DISOUT, CCNO, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1030) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 999
            END IF
C                                       is this an inplace transfer?
         INPLAC = (CCNO.EQ.OLDCNO) .AND. (DISOUT.EQ.DISKIN)
C                                       Only overwrite input file
         IF (.NOT.INPLAC) THEN
            WRITE (MSGTXT,1040)
            CALL MSGWRT (8)
            JERR = 8
            GO TO 999
            END IF
C                                        Update existing CATBLK
         FRW(2) = 2
         CALL CATIO ('READ', DISOUT, CCNO, CATBLK, 'REST', BUFFER, IERR)
         IF (IERR.EQ.0) THEN
            CALL CHR2H (2, SORT(1:2), 1, CATH(KITYP))
            CALL CATIO ('WRIT', DISOUT, CCNO, CATBLK, 'WRIT', BUFFER,
     *         IERR)
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (5)
            END IF
         END IF
      NCFILE = 2
      FVOL(2) = DISOUT
      FCNO(2) = CCNO
      FRW(2) = FRW(2) - 1
C                                       Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFFER, IERR)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISOUT, CCNO, IERR)
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, OLDCNO, CCNO, CATBLK,
     *   BUFFER, BUFFER(257), IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (5)
C                                       New history
      ELSE
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *      BUFFER(257), IERR)
         IF (IERR.EQ.0) THEN
            CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISOUT,
     *         LUN2, BUFFER(257), IERR)
C                                       Add new sort order.
            IF (IERR.EQ.0) THEN
               WRITE (HILINE,1070) TSKNAM, XSORT1, XSORT2
               CALL HIADD (LUN2, HILINE, BUFFER(257), IERR)
               END IF
            END IF
         END IF
C                                       Close history file.
      CALL HICLOS (LUN2, T, BUFFER(257), IERR)
      SEQOUT = CATBLK(KIIMS)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTAB, LUN1, LUN2, DISKIN, DISOUT, OLDCNO,
     *   CCNO, CATBLK, BUFFER, BUFFER(257), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MSRTIN: ERROR',I7,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I7,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('ERROR',I7,' COPYING CATBLK ')
 1030 FORMAT ('ERROR',I7,' CREATING OUTPUT FILE')
 1040 FORMAT ('OUTPUT FILE ALREADY EXISTS, BUT IS NOT INPUT FILE')
 1050 FORMAT ('MSRTIN: ERROR',I7,' UPDATING NEW CATBLK')
 1060 FORMAT ('MSRTIN: ERROR',I7,' COPY/OPEN HISTORY FILE')
 1070 FORMAT (A6,' SORT = ','''',2A1,''' / New sort order')
 1100 FORMAT ('MSRTIN: ERROR ',I7,' COPYING TABLES')
      END
      SUBROUTINE INMSRT (JERR)
C-----------------------------------------------------------------------
C   INMSRT merely implements dynamic memory allocation for IMSORT.
C   Inputs: ----
C   Output: JERR   I     Error code: 0 => okay, else quit
C-----------------------------------------------------------------------
      INTEGER   NMEM, JERR, I, IP(2)
      LONGINT   OKEYS, OIP, OWK
      REAL      KEYS(2), WK(2), PP(2)
      EQUIVALENCE (PP(1), IP(1))
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      JERR = 0
      CALL UVPGET(JERR)
      IF (JERR.NE.0) GO TO 100

      NMEM = NVIS*3
      NMEM = ( ( NMEM - 1 )/256 + 1 ) / 4 + 1
      CALL ZMEMRY ('GET', 'INMSRT', NMEM, KEYS, OKEYS, JERR)
      IF (JERR.NE.0) GO TO 100
      NMEM = NVIS + 2
      NMEM = ( ( NMEM - 1 )/256 + 1 ) / 4 + 1
      CALL ZMEMRY ('GET', 'INMSRT', NMEM, PP, OIP, JERR)
      IF (JERR.NE.0) GO TO 100
C                                       This used to be (NVIS+2)*3
C                                       but i think this is all thats
C                                       needed!
      NMEM = (NVIS + 2) * 2
      NMEM = ( ( NMEM - 1 )/256 + 1 ) / 4 + 1
      CALL ZMEMRY ('GET', 'INMSRT', NMEM, WK, OWK, JERR)
      IF (JERR.NE.0) GO TO 100

      CALL IMSORT (KEYS(1+OKEYS), WK(1+OWK), IP(1+OIP), JERR)
C
 100  CALL ZMEMRY ('FRAL', 'INMSRT', NMEM, KEYS, OKEYS, I)
C
 999  RETURN
      END
      SUBROUTINE IMSORT (KEYS, WK, IP, JERR)
C-----------------------------------------------------------------------
C   INMSRT reads an input file, and sorts it to an output file which
C   may possibly be the same file
C   Input:
C      KEYS, WK, IP, declared outside for use inside this routine
C   Output:
C      JERR     I    Return error code.: 0 = OK
C                      3 = problem with output file
C                      4 = sort failure.
C                      8 = I/O error
C-----------------------------------------------------------------------
      INTEGER   IP(*), JERR
      REAL      KEYS (3,*), WK(2,*)
C
      INTEGER   BUFSZ
      PARAMETER (BUFSZ=8*262144)
      INTEGER   IERR, OUTOFP, LUN1, LUN2, IND1, IND2, FIRSTK, BUFHF,
     *   NIO, SEP, KIO, KBUFHF
      LOGICAL   T, F, FORCED
      REAL      BUFFER(BUFSZ)
      CHARACTER PHNAME*48
      INCLUDE 'MSORT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Murphy's Law
      JERR = 8
C                                       open input file
      LUN1 = 16
      CALL ZPHFIL ('UV', DISKIN, FCNO(1), 1, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZOPEN (LUN1, IND1, DISKIN, PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         JERR = 3
         GO TO 999
         END IF
C                                       open output file
      LUN2 = 17
      CALL ZPHFIL ('UV', DISOUT, FCNO(2), 1, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZOPEN (LUN2, IND2, DISOUT, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         JERR = 3
         GO TO 999
         END IF
C                                       construct sorted key-list
      KBUFHF = MIN (262144, BUFSZ)
      KIO = (KBUFHF - NBPS) / LREC
      IF (KIO.LE.0) THEN
         MSGTXT = 'KEY READ BUFFER TOO SMALL'
         JERR = 1
         GO TO 990
         END IF
C                                       reserve one record
C                                       divide buffer in half
      BUFHF = BUFSZ - LREC - 2*NBPS
      BUFHF = BUFHF / 2
      NIO = ( BUFHF - 2*NBPS ) / LREC
      IF (NIO.LE.0) THEN
         JERR = 1
         MSGTXT = 'SORT BUFFER TOO SMALL'
         GO TO 990
         END IF
      FORCED = (.NOT.INPLAC) .AND. (LREC.GE.NBPS/2)
      CALL GKEYS (KIO, KBUFHF, LUN1, IND1, BUFFER, SORT1, SORT2, WK, IP,
     *   KEYS, NIO, FORCED, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         JERR = 4
         GO TO 990
         END IF
C                                       how bad is it?
      CALL CHKKEY (KEYS, 1, NVIS, OUTOFP, FIRSTK, SEP)
      IF (PRTLV.GE.0) THEN
         WRITE (MSGTXT,1100) OUTOFP, FIRSTK, SEP
         CALL MSGWRT (3)
         END IF
C                                       Brute force file copy will work
      IF (FORCED) THEN
         MSGTXT = 'IMSORT: Begin brute disk read sort pass'
         CALL MSGWRT (2)
         CALL DISKIT (LUN1, LUN2, IND1, IND2, NIO, LREC, NBPS, KEYS,
     *      NVIS, PRTLV, BUFFER, BUFHF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            JERR = 4
            GO TO 990
            END IF
C                                       try neighbor-sort if needed or
C                                       if file copy needed
      ELSE
         IF ((OUTOFP.GT.0) .OR. (.NOT.INPLAC)) THEN
            MSGTXT = 'IMSORT: Begin first (neighbor) sort pass'
            CALL MSGWRT (2)
            CALL NEIGHB (LUN1, LUN2, IND1, IND2, NIO, LREC, NBPS, KEYS,
     *         NVIS, PRTLV, BUFFER, BUFHF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1120) IERR
               JERR = 4
               GO TO 990
               END IF
            END IF
C                                       done?
         CALL CHKKEY (KEYS, 1, NVIS, OUTOFP, FIRSTK, SEP)
         IF (OUTOFP.EQ.0) GO TO 980
         IF (PRTLV.GE.0) THEN
            WRITE (MSGTXT,1100) OUTOFP, FIRSTK, SEP
            CALL MSGWRT (3)
            END IF
C                                       Close files.
         CALL ZCLOSE (LUN2, IND2, IERR)
         CALL ZCLOSE (LUN1, IND1, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       open output file
         LUN1 = 16
         CALL ZPHFIL ('UV', DISOUT, FCNO(2), 1, PHNAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZOPEN (LUN1, IND1, DISOUT, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            JERR = 3
            GO TO 999
            END IF
C                                       open output file
         LUN2 = 17
         CALL ZPHFIL ('UV', DISOUT, FCNO(2), 1, PHNAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZOPEN (LUN2, IND2, DISOUT, PHNAME, T, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
            JERR = 3
            GO TO 999
            END IF
C                                       try brute-force sort
         MSGTXT = 'IMSORT: Begin second (brute-force) sort pass'
         CALL MSGWRT (3)
         CALL BFORCE (LUN1, LUN2, IND1, IND2, NIO, LREC, NBPS, KEYS,
     *      NVIS, BUFFER, BUFHF, PRTLV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1210) IERR
            JERR = 4
            GO TO 990
            END IF
C                                       Close files.
C                                       done?
         CALL CHKKEY (KEYS, 1, NVIS, OUTOFP, FIRSTK, SEP)
         IF (OUTOFP.EQ.0) GO TO 980
         IF (PRTLV.GE.0) THEN
            WRITE (MSGTXT,1100) OUTOFP, FIRSTK, SEP
            CALL MSGWRT (6)
            END IF
C                                       give up and die
         WRITE (MSGTXT,1300) OUTOFP
         JERR = 4
         GO TO 990
         END IF
C                                       everything went okay.
 980  JERR = 0
 990  IF (JERR.NE.0) CALL MSGWRT (8)
      CALL ZCLOSE (LUN2, IND2, IERR)
      IF (IERR.EQ.0) CALL ZCLOSE (LUN1, IND1, IERR)
      IF (IERR.NE.0) JERR = 8
C                                       return here
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMSORT: OPEN ERROR',I3,' ON INPUT FILE')
 1010 FORMAT ('IMSORT: OPEN ERROR',I3,' ON OUTPUT FILE')
 1020 FORMAT ('IMSORT: ERROR ',I3,' COLLECTING SORT KEYS')
 1100 FORMAT (I10,' unsorted starting at',I10,' avg sep',I9)
 1110 FORMAT ('IMSORT: ERROR ',I3,' DURING BRUTE DISK SORT')
 1120 FORMAT ('IMSORT: ERROR ',I3,' DURING NEIGHBOR SORT')
 1210 FORMAT ('IMSORT: ERROR ',I3,' DURING BRUTE FORCE SORT')
 1300 FORMAT ('MSORT ALGORITHM FAILED! ',I5,' unsorted, dump core!')
      END
      SUBROUTINE GKEYS (NIO, BUFHF, LUN1, IND1, BUFFER, SORT1, SORT2,
     *   WK, IP, KEYS, SIO, FORCED, IERR)
C-----------------------------------------------------------------------
C   GKEYS loads the KEYS buffer with the values of the keys over
C   the file is to be sorted.
C   Inputs(from COMMON):
C      NVIS      I           number of visibilities in the input file
C   Inputs:
C      NIO       I           Number of data records per I/O here
C      BUFHF     I           size of the scratch buffer in words
C      LUN1      I           file access variables
C      IND1      I
C      BUFFER    R(BUFHF)    Scratch buffer for moving records
C      SORT1     I           Primary sort key [varies slowest]
C      SORT2     I           Secondary sort key [varies fastest]
C      WK        R(2,NVIS)   Work vectors for ICSORT
C      IP        I(NVIS+2)   Work vectors for ICSORT
C      SIO       I           Number of vis in sort buffer
C   In/out:
C      FORCED    L           Should we do straight random copy?
C   Output:
C      KEYS      R(3,*)      array of keys, 3 per record, shows sort
C                            status of file
C      IERR                  Error Code: 0 => OK
C-----------------------------------------------------------------------
      INTEGER   NIO, BUFHF, LUN1, IND1, SORT1, SORT2, IP(*), SIO, IERR
      REAL      WK(2,*), KEYS(3,*), BUFFER(BUFHF)
      LOGICAL   FORCED
C
      INTEGER   KEY1, KEY2, KEYU, KEYV, BUFUSE, B0, VOFF, BIND1, J, I,
     *   OUTOFP, FIRSTK, SEP
      REAL      XX, YY
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       get keys here
      IF (SORT1.EQ.1) THEN
         KEY1 = ILOCB
         IF (KEY1.LT.0) KEY1 = -10
         END IF
      IF (SORT1.EQ.2) KEY1 = ILOCT
      IF (SORT1.EQ.3) KEY1 = ILOCU
      IF (SORT1.EQ.4) KEY1 = ILOCV
      IF (SORT1.EQ.5) KEY1 = ILOCW
      IF (SORT1.EQ.6) KEY1 = -1
      IF (SORT1.EQ.7) KEY1 = -2
      IF (SORT1.EQ.8) KEY1 = -3
      IF (SORT1.EQ.9) KEY1 = -4
      IF (SORT1.EQ.10) KEY1 = -5
      IF (SORT1.EQ.11) KEY1 = -6
      IF (SORT2.EQ.1) THEN
         KEY2 = ILOCB
         IF (KEY2.LT.0) KEY2 = -10
         END IF
      IF (SORT2.EQ.2) KEY2 = ILOCT
      IF (SORT2.EQ.3) KEY2 = ILOCU
      IF (SORT2.EQ.4) KEY2 = ILOCV
      IF (SORT2.EQ.5) KEY2 = ILOCW
      IF (SORT2.EQ.6) KEY2 = -1
      IF (SORT2.EQ.7) KEY2 = -2
      IF (SORT2.EQ.8) KEY2 = -3
      IF (SORT2.EQ.9) KEY2 = -4
      IF (SORT2.EQ.10) KEY2 = -5
      IF (SORT2.EQ.11) KEY2 = -6
      KEYU = ILOCU
      KEYV = ILOCV
      MSGTXT = 'Read UV data for the sort keys'
      CALL MSGWRT (2)
C                                       get length of buffer in bytes
      BUFUSE = 2 * BUFHF
C                                       Init input file
C                                       init VOFF as 1-rel
      VOFF = 0
C                                       start at first block of file
      IERR = 0
      B0 = 1
      CALL UVINIT ('READ', LUN1, IND1, NVIS, VOFF, LREC, NIO, BUFUSE,
     *   BUFFER, B0, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                      Loop over visibilities
 100  CONTINUE
C
      CALL UVDISK('READ', LUN1, IND1, BUFFER, NIO, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DO 200 I = 1,NIO
C                                       CURRENT record at this POSITION
         J = (I-1)*LREC + BIND1
C                                       this Position
         KEYS(1,VOFF + I) = -(VOFF + I)
C                                       Key 1
         IF (KEY1.GT.0) THEN
            XX = BUFFER (J + KEY1)
         ELSE
            XX = BUFFER (J + KEYU)
            YY = BUFFER (J + KEYV)
            IF (KEY1.EQ.-1) XX = (XX*XX + YY*YY)
            IF (KEY1.EQ.-2) XX = ATAN2 (YY,XX+1.0E-20)
            IF (KEY1.EQ.-3) XX = -ABS(XX)
            IF (KEY1.EQ.-4) XX = -ABS(YY)
            IF (KEY1.EQ.-5) XX = ABS(XX)
            IF (KEY1.EQ.-6) XX = ABS(YY)
            IF (KEY1.EQ.-10) XX = 4096.0 * BUFFER(J+ILOCA1) +
     *         BUFFER(J+ILOCA2)
            END IF
         KEYS(2,VOFF + I) = -XX
C                                       Key 2
         IF (KEY2.GT.0) THEN
            XX = BUFFER (J + KEY2)
         ELSE
            XX = BUFFER (J + KEYU)
            YY = BUFFER (J + KEYV)
            IF (KEY2.EQ.-1) XX = (XX*XX + YY*YY)
            IF (KEY2.EQ.-2) XX = ATAN2 (YY,XX+1.0E-20)
            IF (KEY2.EQ.-3) XX = -ABS(XX)
            IF (KEY2.EQ.-4) XX = -ABS(YY)
            IF (KEY2.EQ.-5) XX = ABS(XX)
            IF (KEY2.EQ.-6) XX = ABS(YY)
            IF (KEY2.EQ.-10) XX = 4096.0 * BUFFER(J+ILOCA1) +
     *         BUFFER(J+ILOCA2)
            END IF
         KEYS(3,VOFF + I) = -XX
 200     CONTINUE
C                                       Increment vis counter
      VOFF = VOFF + NIO
C                                       Get more vis?
      IF (VOFF.LT.NVIS) GO TO 100
C                                       Sort on key1 and key2
      MSGTXT = 'Keys read - sort them'
      CALL MSGWRT (2)
      KEY1 = 2
      KEY2 = 3
      CALL ICSORT (KEYS, 3, NVIS, KEY1, KEY2, WK, IP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Change key 2 to the position in
C                                       sorted file
      DO 250 I = 1,NVIS
         KEYS(2,I) = -I
 250     CONTINUE
C                                       Check them
      IF (FORCED) THEN
         CALL CHKKEY (KEYS, 1, NVIS, OUTOFP, FIRSTK, SEP)
         FORCED = SEP.GT.8*SIO
         END IF
C                                       Report
      IF (FORCED) THEN
          MSGTXT = 'Truely brute force disk copy to be done'
          CALL MSGWRT (2)
C                                       RESORT TO current POS. IN FILE
      ELSE
         MSGTXT = 'Keys sorted, order established, now resort'
         CALL MSGWRT (2)
         KEY1 = 1
         KEY2 = 2
         CALL ICSORT (KEYS, 3, NVIS, KEY1, KEY2, WK, IP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1210) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         MSGTXT = 'Keys now ready to use'
         CALL MSGWRT (2)
         END IF
C                                       make all keys positive now!
      DO 260 I = 1,NVIS
         KEYS(1,I) = -KEYS(1,I)
         KEYS(2,I) = -KEYS(2,I)
         KEYS(3,I) = -KEYS(3,I)
 260     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GKEYS: ERROR ',I10,' ON INIT OF INPUT FILE')
 1100 FORMAT ('GKEYS: ERROR ',I10,' ON READING INPUT FILE')
 1200 FORMAT ('GKEYS: ERROR ',I10,' DURING IN CORE SORT')
 1210 FORMAT ('GKEYS: ERROR ',I10,' DURING SECOND IN CORE SORT')
      END
      SUBROUTINE CHKKEY (KEYS, M, N, OUTOFP, FIRSTK, SEP)
C-----------------------------------------------------------------------
C   CHKKEY compares positions 1 and 2 of the KEYS variable over the
C   range M to N to determine the number of KEYS that are still
C   mis-sorted
C   Inputs:
C      KEYS     R(3,*)   array of keys
C      M        I        first key in KEYS over which order is checked
C      N        I        last key in KEYS over which order is checked
C   Output:
C      OUTOFP   I        number of mis-sorted keys in the range M to N
C      FIRSTK   I        position of first mis-sorted record in KEYS
C      SEP      I        Average absolute value separation
C-----------------------------------------------------------------------
      REAL      KEYS(3,*)
      INTEGER   M, N, OUTOFP, FIRSTK, SEP
C
      INTEGER   I
      REAL      S
C-----------------------------------------------------------------------
      OUTOFP = 0
      FIRSTK = N + 1
      S = 0.0
      DO 100 I = N,M,-1
         IF (KEYS(1,I).NE.KEYS(2,I)) THEN
            S = S + ABS (KEYS(1,I)-KEYS(2,I))
            OUTOFP = OUTOFP + 1
            FIRSTK = I
            END IF
 100     CONTINUE
      SEP = S / (N - M + 1) + 0.5
C
 999  RETURN
      END
      SUBROUTINE ICSORT (A, LEN, N, KEY1, KEY2, WK, IP, IERR)
C-----------------------------------------------------------------------
C   ICSORT does an incore two key sort of data into descending order.
C   The method used depends on LEN and the success of a previously
C   attempted method.
C   Input:
C      A(LEN,N)        R   Data array to be sorted.
C      LEN             I   Length of record in I   words.
C      N               I   Number of records.
C      KEY1,KEY2       I   Locations of Keys in record, KEY2 varies
C                          fastest
C      WK(2,N+2)       R   work vector for OSORT
C      IP(N+2)         R   work vector for OSORT
C   Output:
C      A(LEN,N)        R   Data array sorted.
C      IERR            I   Return code: 0 = OK
C                          1 = Bad input
C-----------------------------------------------------------------------
      INTEGER   LEN, N, NP2, KEY1, KEY2, IERR, JERR, IP(*)
      REAL      A(LEN,N), WK(2,*)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check that input OK.
      IERR = 0
      IF ((LEN.LE.0) .OR. (N.LE.0) .OR. (KEY1.GT.LEN) .OR.
     *   (KEY2.GT.LEN)) IERR = 1
      IF ((KEY1.LT.0) .OR. (KEY2.LT.0)) IERR = 1
      IF (IERR.NE.0) GO TO 999
C                                       If N = 1 return
      IF (N.LE.1) GO TO 999
      NP2 = N + 2
C                                       Check if long record, if so
C                                       use min. switch method.
C                                       Try quick sort
      IF (LEN.GT.2048) GO TO 100
         CALL OSORT (A, N, NP2, KEY1, KEY2, LEN, WK, IP, JERR)
         IF (JERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1000) JERR
            CALL MSGWRT (6)
C                                       Try shell sort
            CALL SHSORT (A, N, KEY1, KEY2, LEN, JERR)
            IF (JERR.EQ.0) GO TO 999
               WRITE (MSGTXT,1001) JERR
               CALL MSGWRT (6)
C                                       Long record sort.
 100  CONTINUE
         CALL LSORT (A, N, KEY1, KEY2, LEN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ICSORT: QUICK SORT ERROR',I3,' TRY SHELL SORT')
 1001 FORMAT ('ICSORT: SHELL SORT ERROR',I3,' TRY LONG SORT')
      END
      SUBROUTINE DISKIT (LUN1, LUN2, IND1, IND2, NIO, LREC, XTRA,
     *   KEYS, NVIS, PRTLV, BUFFER, BUFHF, IERR)
C-----------------------------------------------------------------------
C   DISKIT is a record-moving algorithm.  Given an array KEYS whose
C   first element indicates a record number in the file and whose
C   second element indicates where that record belongs in the file,
C   DISKIT attempts to move that record to its proper location.  This
C   is done by brute force.  It is assumed that KEYS(2,*) is a simple
C   count from 1 to NVIS and that the "random" input file record number
C   is in KEYS(1,*).  That record is read and pushed to the output
C   buffer.  This can only work if the two files are NOT the same.
C   Inputs:
C      LUN1     I
C      IND1     I
C      LUN2     I
C      IND2     I
C      NIO      I        Number logicals per buffer
C      LREC     I        Length of data record in real values
C      XTRA     I        Slop to allow in buffers (NBPS)
C      KEYS     I(3,*)   array of keys, 3 keys for each record in file
C      NVIS     I        number of vis data recors in file
C      PRTLV    I        Print progress?
C      BUFHF    I        Length of BUFFER in words
C   Output:
C      BUFFER   R(*)     Scratch buffer for moving records
C      IERR     I        Error Code 0=> ok.
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, IND1, IND2, NIO, LREC, XTRA, NVIS, BUFHF,
     *   PRTLV, IERR
      REAL      KEYS(3,*), BUFFER(*)
C
      INTEGER   BUFUSE, BIND1, NIO1, VOFF1, B2, BIND2, NIO2, VOFF2, B0,
     *   IPTRO, NIOUT, IPTRI, I, INCR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      B2 = LREC + BUFHF + 1
C                                       Open output
      B0 = 1
      VOFF1 = 0
      NIO1 = MIN (NVIS, NIO)
      BUFUSE = 2 * BUFHF
      CALL UVINIT ('WRIT', LUN2, IND2, NVIS, VOFF1, LREC, NIO1, BUFUSE,
     *   BUFFER, B0, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT WRITE'
         GO TO 990
         END IF
      IPTRO = BIND1
      NIOUT = 0
      INCR = NVIS / 20
C                                       loop over vis reading random
C                                       record to go to out rec I
      DO 100 I = 1,NVIS
         IF ((PRTLV.GT.0) .AND. (MOD(I,INCR).EQ.0)) THEN
            WRITE (MSGTXT,1010) I
            CALL MSGWRT (2)
            END IF
         VOFF2 = KEYS(1,I) - 1
         NIO2 = 1
         BUFUSE = 2 * (LREC + XTRA)
         CALL GBLOCK (LUN1, IND1, NVIS, LREC, BUFFER(B2), BUFUSE, BIND2,
     *      VOFF2, NIO2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ DATA', VOFF2
            GO TO 990
            END IF
         IF (NIO2.NE.1) THEN
            WRITE (MSGTXT,1020) NIO2, 'VIS READ'
            IERR = 1
            GO TO 990
            END IF
         IPTRI = B2 + BIND2 - 1
         CALL RCOPY (LREC, BUFFER(IPTRI), BUFFER(IPTRO))
         IPTRO = IPTRO + LREC
         NIOUT = NIOUT + 1
         IF (NIOUT.GE.NIO1) THEN
            CALL UVDISK ('WRIT', LUN2, IND2, BUFFER, NIO1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITE DATA'
               GO TO 990
               END IF
            IPTRO = BIND1
            NIOUT = 0
            END IF
 100     CONTINUE
      NIOUT = -NIOUT
      CALL UVDISK ('FLSH', LUN2, IND2, BUFFER, NIOUT, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FLUSH DATA'
         GO TO 990
         END IF
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DISKIT: ERROR',I5,1X,A,I10)
 1010 FORMAT ('DISKIT: at vis',I10)
 1020 FORMAT ('DISKIT: READ',I5,' VIS, SHOULD HAVE BEEN 1')
      END
      SUBROUTINE NEIGHB (LUN1, LUN2, IND1, IND2, NIO, LREC, XTRA,
     *   KEYS, NVIS, PRTLV, BUFFER, BUFHF, IERR)
C-----------------------------------------------------------------------
C   NEIGHB is a record-moving algorithm.  Given an array KEYS whose
C   first element indicates a record number in the file and whose
C   second element indicates where that record belongs in the file,
C   NEIGHB attempts to move that record to its proper location.  This
C   is done only if the record is within a critical distance of its
C   desired location, otherwise, the record is left in its (incorrect)
C   position.  Note:  NEIGHB can operate with an input file that is/isnt
C   the same as the output file.
C   Inputs:
C      LUN1           I
C      IND1           I
C      LUN2           I
C      IND2           I
C      BUFFER(*)      R    Scratch buffer for moving records
C      BUFHF          I    Length of BUFFER in words
C      LREC           I    Length of data record in real values
C      KEYS(3,*)      I    array of keys, 3 keys for each record in file
C      NVIS           I    number of vis data recors in file
C   Output:
C      KEYS(3,*)      I    array of keys, with current sort status of
C                          file
C      IERR           I    Error Code 0=> ok.
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, IND1, IND2, NIO, LREC, XTRA, NVIS, BUFHF,
     *   PRTLV, IERR
      REAL      KEYS(3,*), BUFFER(LREC+2*BUFHF)
C
      INTEGER   B, BUFUSE, B1, BIND1, NIO1, VOFF1, B2, BIND2, NIO2,
     *   VOFF2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       define buffer offsets
      B1 = LREC + 1
      B2 = LREC + BUFHF + 1
C                                       start at beginning of file
      VOFF1 = 0
      NIO1  = 0
      VOFF2 = 0
      NIO2  = 0
      VOFF2 = VOFF2 + NIO2

      NIO2 = MIN (NVIS - VOFF2, NIO)
      IF (NIO2.LE.0) GO TO 999
      BUFUSE = 2 * MIN (BUFHF, NIO2 * LREC + XTRA)
      CALL GBLOCK (LUN1, IND1, NVIS, LREC, BUFFER(B2), BUFUSE, BIND2,
     *   VOFF2, NIO2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (NIO2.LE.0) GO TO 999

      CALL NIESW (KEYS, BUFFER(1), LREC, 0,0,0, B2+BIND2-1, VOFF2, NIO2)
C
 100  CONTINUE
C                                       get another buffer-ful?
      IF ((1+VOFF2+NIO2).LT.NVIS) THEN
C                                       shift upper buffer to lower buffer
         B  = B1
         B1 = B2
         B2 = B
         BIND1 = BIND2
         NIO1  = NIO2
         VOFF1 = VOFF2
         VOFF2 = VOFF2 + NIO2
         NIO2  = MIN (NVIS - VOFF2, NIO)
         IF (NIO2.LE.0) GO TO 999
         BUFUSE = 2 * MIN (BUFHF, NIO2 * LREC + XTRA)
         CALL GBLOCK (LUN1, IND1, NVIS, LREC, BUFFER(B2), BUFUSE, BIND2,
     *      VOFF2, NIO2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF (NIO2.LE.0) GO TO 999
         IF (PRTLV.GT.0) THEN
            WRITE (MSGTXT,1100) VOFF2+1
            CALL MSGWRT (2)
            END IF
C                                       sort locally
         CALL NIESW (KEYS, BUFFER(1), LREC, B1+BIND1-1, VOFF1, NIO1,
     *      B2+BIND2-1, VOFF2, NIO2)
         BUFUSE = 2 * BUFHF
         CALL WBLOCK (LUN2, IND2, NVIS, VOFF1, LREC, NIO1, BUFFER(B1),
     *      BUFUSE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         GO TO 100
         END IF
C                                       send out upper buffer
      BUFUSE = 2 * BUFHF
      CALL WBLOCK (LUN2, IND2, NVIS, VOFF2, LREC, NIO2, BUFFER(B2),
     *   BUFUSE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NEIGHB: ERROR ',I10,' READING INPUT FILE')
 1010 FORMAT ('NEIGHB: ERROR ',I10,' WRITING OUTPUT FILE')
 1100 FORMAT ('NEIGHB: at vis',I10)
      END
      SUBROUTINE NIESW (KEYS, SBUF, LREC, BOFF1, VOFF1, NIO1, BOFF2,
     *   VOFF2, NIO2)
C-----------------------------------------------------------------------
C   NIESW swaps records in two blocks of the memory buffer SBUF,
C   NIESW checks the desired position of each record in SBUF and
C   swaps those whose desired positions are currently in memroy.
C   The two blocks need not correspond to adjacent portions of
C   the data file; however, the second block must not overlap the
C   first block and must not proceed it either.
C   Inputs:
C      KEYS       R(3,*)   array of keys, contains sort status of file
C      SBUF       R(*)     scratch buffer where records are located
C      LREC       I        length of each record in real values
C      BOFF1      I        value offset in lower buffer, 1-relative
C      BOFF2      I        value offset in upper buffer, 1-relative
C      VOFF1      I        record offset of lower buffer in file
C      VOFF2      I        record offset of upper buffer in file
C      NIO1       I        number of records in lower buffer
C      NIO2       I        number of records in upper buffer
C   Output:
C      IERR       I        Error Code:
C                            0 => ok
C                            1 => not properly ordered in file
C-----------------------------------------------------------------------
      REAL      KEYS(3,*), SBUF(*)
      INTEGER   LREC, VOFF1, VOFF2, NIO1, NIO2, BOFF1, BOFF2, IERR
C
      INTEGER   HA, WA, IHA, IWA, I
      REAL      TKEY
C-----------------------------------------------------------------------
      IERR = 0
C                                       second buffer must be non-empty
      IF (NIO2.LE.0) GO TO 999
C                                       insist on strict ordering
      IF ((VOFF1+NIO1).GT.VOFF2) THEN
         IERR = 1
         GO TO 999
         END IF
C                                       start at first buffer
      IF (NIO1.GT.0) I = VOFF1 + 1
C                                       if its empty, skip it
      IF (NIO1.EQ.0) I = VOFF2 + 1
 200  CONTINUE
         HA = I
         WA = KEYS(2,HA)
         IHA = -1
         IWA = -1
C                                       records are different
         IF (WA.NE.HA) THEN
            IF ((HA.GT.VOFF1) .AND. (HA.LE.(VOFF1 + NIO1)))
     *         IHA = (HA - VOFF1 - 1) * LREC + BOFF1
            IF ((HA.GT.VOFF2) .AND. (HA.LE.(VOFF2 + NIO2)))
     *         IHA = (HA - VOFF2 - 1) * LREC + BOFF2
            IF ((WA.GT.VOFF1) .AND. (WA.LE.(VOFF1 + NIO1)))
     *         IWA = (WA - VOFF1 - 1) * LREC + BOFF1
            IF ((WA.GT.VOFF2) .AND. (WA.LE.(VOFF2 + NIO2)))
     *         IWA = (WA - VOFF2 - 1) * LREC + BOFF2
C                                       both are present
            IF ((IHA.GE.0) .AND. (IWA.GE.0)) THEN
               CALL RCOPY (LREC, SBUF(IWA), SBUF( 1 ))
               CALL RCOPY (LREC, SBUF(IHA), SBUF(IWA))
               CALL RCOPY (LREC, SBUF( 1 ), SBUF(IHA))
               TKEY = KEYS(3,HA)
               KEYS(3,HA) = KEYS(3,WA)
               KEYS(3,WA) = TKEY
               TKEY = KEYS(2,HA)
               KEYS(2,HA) = KEYS(2,WA)
               KEYS(2,WA) = TKEY
C                                       check 'new' current record
               GO TO 200
               END IF
            END IF
C                                       help jump any possible gap
         IF (I.EQ.(VOFF1+NIO1)) I = VOFF2
         I = I + 1
         IF (I.LE.(VOFF2+NIO2)) GO TO 200
C
 999  RETURN
      END
      SUBROUTINE BFORCE (LUN1, LUN2, IND1, IND2, NIO, LREC, XTRA, KEYS,
     *   NVIS, BUFFER, BUFHF, PRTLV, IERR)
C-----------------------------------------------------------------------
C   BFORCE is a record-moving algorithm that theoretically cannot fail.
C   It loads records into memory two blocks at a time and directly moves
C   all records to their desired position in the file.  Since each block
C   might need to send records to each other block, its possible that
C   this algorithm will load pairs of blocks of order the square of the
C   number of blocks in the file.  Since the routine ZMI2 limits the
C   amount of data that can be retrieved, the block size in the data
C   has a fixed upper limit, for a given data file.  NOTE: This file
C   requires that the input file be the same as the output file!
C   Inputs:
C      LUN1           I
C      IND1           I
C      LUN2           I
C      IND2           I
C      BUFFER(*)      R    Scratch buffer for moving records
C      BUFHF         I    Length of BUFFER in AIPS-bytes
C      LREC           I    Length of data record in real values
C      KEYS(3,*)      I    array of keys, 3 keys for each record in file
C      NVIS           I    number of vis data recors in file
C   Output:
C      KEYS(3,*)      I    array of keys, with current sort status of
C                          file
C      IERR           I    Error Code 0=> ok.
C-----------------------------------------------------------------------
      INTEGER LUN1, LUN2, IND1, IND2, LREC, BUFHF, IERR, NVIS, PRTLV
      INTEGER NIO, XTRA
      REAL KEYS(3,*), BUFFER(LREC+2*BUFHF)
C
      INTEGER   BUFUSE, FRONT, BACK, FIRSTK, OUTOFP, SEP, B1, VOFF1,
     *   NIO1, BIND1, B2, VOFF2, NIO2, BIND2
      LOGICAL   FIRST
      INCLUDE 'INCS:DMSG.INC'
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       define buffer offsets
      B1 = LREC + 1
      B2 = LREC + BUFHF + 1
C                                       start at beginning of file
      NIO2  = 0
      VOFF2 = 0
 300  CONTINUE
      IF ((VOFF2+1).LT.NVIS) THEN
C                                       skip some at beginning!
         IF (KEYS(2,VOFF2+1).EQ.(VOFF2+1)) THEN
            VOFF2 = VOFF2 + 1
            GO TO 300
            END IF
C                                       update user if requested!
         IF ((PRTLV.GE.0) .AND. (.NOT.FIRST)) THEN
            CALL CHKKEY (KEYS, 1, NVIS, OUTOFP, FIRSTK, SEP)
            WRITE (MSGTXT,1100) OUTOFP, FIRSTK, SEP
            CALL MSGWRT (3)
            END IF
         FIRST = .FALSE.
C
         NIO2 = MIN (NVIS - VOFF2, NIO)
         IF (NIO2.LE.0) GO TO 999
         BUFUSE = 2 * MIN (BUFHF, NIO2 * LREC + XTRA)
         CALL GBLOCK (LUN1, IND1, NVIS, LREC, BUFFER(B2), BUFUSE,
     *      BIND2, VOFF2, NIO2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF (IERR.NE.0) GO TO 999
         IF (NIO2.LE.0) GO TO 999
C                                       SWAP RECORDS
         CALL NIESW (KEYS,BUFFER(1), LREC,0,0,0, B2+BIND2-1, VOFF2,NIO2)
C
         NIO1  = 0
         VOFF1 = VOFF2 + NIO2
         FRONT = VOFF1
         BACK  = NVIS
         CALL SWITCH (KEYS, VOFF2+1, VOFF1, FRONT, BACK)
         VOFF1 = MAX(FRONT, VOFF1)
 310     CONTINUE
         IF ((VOFF1+1).LT.BACK) THEN
C
            NIO1 = MIN (NVIS - VOFF1, NIO)
            IF (NIO1.LE.0) GO TO 999
            BUFUSE = 2 * MIN (BUFHF, NIO1 * LREC + XTRA)
            CALL GBLOCK (LUN1, IND1, NVIS, LREC, BUFFER(B1), BUFUSE,
     *         BIND1, VOFF1, NIO1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            IF (IERR.NE.0) GO TO 999
            IF (NIO1.LE.0) GO TO 999
C
            CALL NIESW (KEYS, BUFFER(1), LREC,
     *         B2+BIND2-1, VOFF2, NIO2, B1+BIND1-1, VOFF1, NIO1)
            BUFUSE = 2 * BUFHF
            CALL WBLOCK (LUN2, IND2, NVIS, VOFF1, LREC, NIO1,
     *         BUFFER(B1), BUFUSE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            IF (IERR.NE.0) GO TO 999
            VOFF1 = VOFF1 + NIO1
C                                       get more
            GO TO 310
            END IF
         BUFUSE = 2 * BUFHF
         CALL WBLOCK (LUN2, IND2, NVIS, VOFF2, LREC, NIO2,
     *      BUFFER(B2), BUFUSE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         VOFF2 = VOFF2 + NIO2
C                                       get more
         GO TO 300
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BFORCE: ERROR ',I10,' READING INPUT FILE')
 1010 FORMAT ('BFORCE: ERROR ',I10,' WRITING OUTPUT FILE')
 1100 FORMAT (I10,' unsorted starting at',I10,' avg sep',I9)
      END
      SUBROUTINE GBLOCK (LUN, IND, NVIS, LREC, BUF, LEN, B1, OFF, NIO,
     *   IERR)
C-----------------------------------------------------------------------
C   GBLOCK requests NIO vis. of length LREC starting at vis #OFF
C   from the file described by LUN and IND.  On output, NIO tells how
C   many vis actually were returned (might be smaller than NIO)
C   Inputs:
C      LUN
C      IND
C      NVIS   I     total number of VIS in file
C      LREC   I     length of vis record in values
C      OFF    I     VIS offset in file
C      NIO    I     number of VIS requested
C   Output:
C      NIO    I     number of VIS actually loaded
C      BUF    R(*)  Scratch space with loaded visibilities
C      B1     I     value offset in BUF
C      IERR   I     Error Code: 0=> OK
C-----------------------------------------------------------------------
      INTEGER LUN, IND, NVIS, LREC, LEN, B1, OFF, NIO, IERR
      REAL    BUF(*)
C
      INTEGER B0
C-----------------------------------------------------------------------
      IERR = 0
      B0 = 1
      CALL UVINIT ('READ', LUN, IND, NVIS, OFF, LREC, NIO, LEN, BUF, B0,
     *   B1, IERR)
      IF (IERR.EQ.0) CALL UVDISK ('READ', LUN, IND, BUF, NIO, B1, IERR)
C
 999  RETURN
      END
      SUBROUTINE WBLOCK (LUN, IND, NVIS, OFF, LREC, NVIO, BUF, LEN,
     *   IERR)
C-----------------------------------------------------------------------
C   WBLOCK tries to write NVIO vis. of length LREC starting at vis #OFF
C   from the file described by LUN and IND.
C   Inputs:
C      LUN
C      IND
C      NVIS   I     total number of VIS in file
C      LREC   I     length of vis record in values
C      OFF    I     VIS offset in file
C      NVIO   I     number of VIS requested to write
C      BUF    R(*)  Scratch space with loaded visibilities
C   Output:
C      IERR   I     Error Code: 0=> OK
C                               1=> incomplete write
C-----------------------------------------------------------------------
      INTEGER LUN, IND, NVIS, OFF, LREC, NVIO, IERR
      REAL    BUF(*)
      INTEGER B0, BIND0, NIO, LEN
C-----------------------------------------------------------------------
      IERR = 0
      B0 = 1
      NIO = NVIO
      CALL UVINIT ('WRIT', LUN, IND, NVIS, OFF, LREC, NIO,
     *   LEN, BUF, B0, BIND0, IERR)
      NIO = -NIO
      CALL UVDISK ('FLSH', LUN, IND, BUF, NIO, BIND0, IERR)
      IF (NIO.NE.0) IERR = 1
C
 999  RETURN
      END
      SUBROUTINE SWITCH (KEYS, M, N, FRONT, BACK)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      REAL    KEYS(3,*)
      INTEGER M, N, I, FRONT, BACK
C-----------------------------------------------------------------------
      DO 100 I = M, N
         IF (KEYS(2,I).NE.I) THEN
            FRONT = MIN ( INT(KEYS(2,I)) , FRONT )
            BACK  = MAX ( INT(KEYS(2,I)) , BACK  )
            END IF
 100     CONTINUE
C
 999  RETURN
      END
