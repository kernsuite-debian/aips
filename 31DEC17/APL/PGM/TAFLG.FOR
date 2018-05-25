LOCAL INCLUDE 'TAFLG.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMIN(3), XCLAIN(2), XINEXT, XOPTYP
      REAL      XSIN, XDISIN, XIVER, XOVER, BCOUNT, ECOUNT, APARM(10),
     *   BPARM(10), CPARM(10), RECR(XBPRSZ)
      CHARACTER NAMEIN*12, CLAIN*6, INEXT*2, XPTYPE*2, OPFILE*48,
     *   TEXT*1024
      DOUBLE PRECISION RSUM(2), DEG2R, RECD(XBPRSZ/2)
      INTEGER   RECI(XBPRSZ), SBUF(256), PBUF(256), TABUF(512), NKEY,
     *   NCOL, NREC, IBCNT, IECNT, IMCNT, NCOUNT, SEQIN, DISKIN, TALUN,
     *   IOP, TYPEAX(2,2), IVER, OVER, CNO, NPARM, DTYPAX(2,2),
     *   ISUBAX(2,2), TAIND, OPTYPE(2)
      LOGICAL   SUMEM(2), TWOOF(2), ABSEM(2,2)
      EQUIVALENCE (RECD, RECR, RECI)
      COMMON /CHPARM/ NAMEIN, CLAIN, INEXT, XPTYPE, TEXT, OPFILE
      COMMON /INPARM/ XNAMIN, XCLAIN, XSIN, XDISIN, XINEXT, XIVER,
     *   XOVER, BCOUNT, ECOUNT, XOPTYP, APARM, BPARM, CPARM
      COMMON /TAFLGP/  RSUM, DEG2R, RECD, SBUF, PBUF, TABUF, NKEY, NCOL,
     *   NREC, IBCNT, IECNT, IMCNT, NCOUNT, SUMEM, TWOOF, ABSEM, SEQIN,
     *   DISKIN, TALUN, IOP, TYPEAX, IVER, OVER, CNO, NPARM, DTYPAX,
     *   ISUBAX, TAIND, OPTYPE
LOCAL END
      PROGRAM TAFLG
C-----------------------------------------------------------------------
C! Task to flag extension tables
C# EXT-util EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007, 2009
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
C   TAFLG flags table extension data.
C   Inputs:
C     INNAME         NAMEIN        Name of input primary file.
C     INCLASS        CLAIN         Class of input primary file.
C     INSEQ          SEQIN         Seq. of input primary file
C     INDISK         DISKIN        Disk number of input primary file.
C     INEXT......Type of input table extension.  '  ' = 'CC'
C     INVERS.....Version number of table extension.  0 => highest.
C     OUTVERS....Version number of output table.  0 => highest + 1.
C     BCOUNT.....Beginning row number to be included in flagging.
C     ECOUNT.....Ending row number included in flagging.  0 = highest.
C     OPTYPE.....Type of comparison: '>','>=','=','<>','<=','<'
C     APARM......Column selection parameters:
C        1 = L-side logical column number A:  0 => row number.
C            < 0 => use absolute value of col. abs(APARM(1))
C        2 = The subscript of the L-side column A data to use if the
C            column is an array.  0 => 1.
C        3 = L-side logical column number B:  0 => row number.
C            < 0 => use absolute value of col. abs(APARM(1))
C        4 = The subscript of the L-side column B data to use if the
C            column is an array.  0 => 1.
C        5 = L-side function number: Various functions may be
C            performed on column A only or on a pair of columns A
C            and B before testing.  Functions which use column A
C            only are 0 = no function, 1 = sum, 2 = asin, 3 = Log10,
C            4 = ln, 5 = exp, 6 = sin, 7 = cos, 8 = tan, 9 = atan.
C            In these cases, APARM(3) and (4) are ignored.
C            Functions which use both columns are 10 = +, 11 = -,
C            12 = *, 13 = /, 14 = **, 15 = mod, 16 = Modulus,
C            17 = atan2, 18 = max, 19 = min.
C        6 = R-side logical column number A:  as for L-side.
C        7 = The subscript of the R-side column A data to use if the
C            column is an array.  0 => 1.
C        8 = R-side logical column number B:  as for L-side.
C        9 = The subscript of the R-side column B data to use if the
C            column is an array.  0 => 1.
C        10 = R-side function number: as for L-side.
C     BPARM......Control parameters:
C        1 = Flag to set R-side to a constant: > 0 use BPARM(2) as a
C            constant R-side; <= 0 compute the function specified in
C            APARM(6) - APARM(10) as the R-side.
C        2 = The constant comparison value for the R-side (used only
C            if (BPARM(1) > 0).
C        3 = For '=' and '<>' tests on floating-point columns or
C            floating-point function results, the allowed difference
C            between the L-side value and the R-side value to be
C            regarded as "equal", i.e. if ABS(L-side - R-side) <
C            BPARM(3) then L-side and R-side are considered equal.
C        4 = <= 0 implies flag the row having the specified parms.
C            > = implies remove flagging from those rows previously
C            flagged which have the specified parms.
C        9 = The exponent to which to take the L-side result value
C            before final scaling and comparison.   0 => 1.
C        10 = The exponent to which to take the R-side result value
C            before comparison.   0 => 1.
C     CPARM......After the value or absolute value of a column is
C             found, it may be scaled and offset before other
C             functions are applied to it.  Similarly, the result
C             on the L side (only) may also be scaled and offset
C             before comparison with the R-side.
C        1 = L-side logical column number A scale factor: 0 -> 1.
C        2 = L-side logical column number A offset.
C        3 = L-side logical column number B scale factor: 0 -> 1.
C        4 = L-side logical column number B offset.
C        5 = R-side logical column number A scale factor: 0 -> 1.
C        6 = R-side logical column number A offset.
C        7 = R-side logical column number B scale factor: 0 -> 1.
C        8 = R-side logical column number B offset.
C        9 = L-side OVERALL result scale factor.  0 -> 1.
C        10 = L-side OVERALL result offset.  0 -> 1.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'TAFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TAFLG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TAFLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do flagging
      CALL TAFLAG (IRET)
      IF (IRET.EQ.0) CALL TAFLHI
C                                       Close down
 990  CALL DIE (IRET, SBUF)
C
 999  STOP
      END
      SUBROUTINE TAFLIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   TAFLIN gets input parameters for TAFLG.   It copies the input table
C   to the output table and then opens the output for read/write.
C   Inputs:  PRGM   C*6)      Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER  INTYPE*2, STAT*4, PRGM*6, PFILE*48, CHAX(2)*2,
     *   OKOPS(6)*4, CHTM12*12
      INTEGER  JERR, IUSER, I, IERR, IROUND, IO, ID1, ID2, T2LUN,
     *    T2IND, IMAXV, ISIZE, OISIZE, INREC, IRNO
      REAL      EPS
      LOGICAL   T, F, EQUAL
      INCLUDE 'TAFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F, CHAX /.TRUE.,.FALSE., 'X-','Y-'/
      DATA OKOPS /'> ','>=','= ','<>','<=','< '/
C-----------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      DEG2R = ATAN(1.0D0) / 45.0D0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 43
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMIN, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, SBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XINEXT, INEXT)
      CALL H2CHR (2, 1, XOPTYP, XPTYPE)
C                                       Crunch input parameters.
      EPS = 0.1
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DO 15 IOP = 1,6
         IF (XPTYPE.EQ.OKOPS(IOP)) GO TO 20
 15      CONTINUE
      WRITE (MSGTXT,1015) XOPTYP
      GO TO 990
C                                       Get CATBLK from file.
 20   CNO = 1
      INTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, INTYPE,
     *   IUSER, STAT, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 25
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, IUSER
         GO TO 990
 25   CALL CATIO ('READ', DISKIN, CNO, CATBLK, 'WRIT', SBUF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1025) IERR
         GO TO 990
 30   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
      XSIN = SEQIN
      XDISIN = DISKIN
C                                       Find extension file
      IF (INEXT.EQ.'  ') INEXT = 'CC'
      IVER = IROUND (XIVER)
      OVER = IROUND (XOVER)
      IF (INEXT.EQ.INTYPE) IVER = 1
      IF (INEXT.EQ.INTYPE) OVER = 1
      CALL FNDEXT (INEXT, CATBLK, IMAXV)
      IF ((OVER.LE.0) .OR. (OVER.GT.IMAXV)) OVER = IMAXV + 1
      TALUN = 28
      T2LUN = 29
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', INEXT, DISKIN, CNO, IVER, CATBLK, T2LUN,
     *   NKEY, NREC, NCOL, PBUF, TABUF, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, INEXT, IVER
         GO TO 990
C                                       Set/check parms
 40   IMCNT = TABUF(5)
      INREC = TABUF(1)
      T2IND = TABUF(82)
      IECNT = IMCNT
      IF ((ECOUNT.GE.1.0) .AND. (ECOUNT.LT.IECNT)) IECNT = ECOUNT + 0.1
      IBCNT = BCOUNT + 0.1
      IF (IBCNT.LE.0) IBCNT = 1
      IF (IBCNT.LE.IECNT) GO TO 45
         WRITE (MSGTXT,1040) IBCNT, IECNT
         GO TO 990
 45   SUMEM(2) = .FALSE.
      DO 60 I = 1,2
         IO = (I-1)*5
         OPTYPE(I) = IROUND (APARM(IO+5))
         IF (OPTYPE(I).LT.0) OPTYPE(I) = 0
         IF ((I.EQ.2) .AND. (BPARM(1).GT.0.0)) GO TO 60
         TWOOF(I) = OPTYPE(I).GT.9
         TYPEAX(1,I) = IROUND (APARM(IO+1))
         TYPEAX(2,I) = 0
         IF (TWOOF(I)) TYPEAX(2,I) = IROUND (APARM(IO+3))
         IF ((TYPEAX(1,I).GE.-NCOL) .AND. (TYPEAX(1,I).LE.NCOL) .AND.
     *      (TYPEAX(2,I).GE.-NCOL) .AND. (TYPEAX(2,I).LE.NCOL)) GO TO 50
            WRITE (MSGTXT,1045) CHAX(I), TYPEAX(1,I), TYPEAX(2,I)
            GO TO 990
 50      SUMEM(I) = OPTYPE(I).EQ.1
         ABSEM(1,I) = TYPEAX(1,I).LT.0
         ABSEM(2,I) = TYPEAX(2,I).LT.0
         TYPEAX(1,I) = ABS(TYPEAX(1,I))
         TYPEAX(2,I) = ABS(TYPEAX(2,I))
         DTYPAX(1,I) = 0
         DTYPAX(2,I) = 0
         ISUBAX(1,I) = APARM(IO+2) + 0.1
         ISUBAX(2,I) = 1
         IF (TWOOF(I)) ISUBAX(2,I) = APARM(IO+4) + 0.1
         IF (TYPEAX(1,I).GT.0) DTYPAX(1,I) = PBUF(128+TYPEAX(1,I))
         IF (TYPEAX(2,I).GT.0) DTYPAX(2,I) = PBUF(128+TYPEAX(2,I))
         ID1 = DTYPAX(1,I) / 10
         ID2 = DTYPAX(2,I) / 10
         IF (ID1.LT.1) ID1 = 1
         IF (ID2.LT.1) ID2 = 1
         DTYPAX(1,I) = MOD (DTYPAX(1,I), 10)
         DTYPAX(2,I) = MOD (DTYPAX(2,I), 10)
         IF ((ISUBAX(1,I).LE.0) .OR. (ID1.EQ.1)) ISUBAX(1,I) = 1
         IF ((ISUBAX(2,I).LE.0) .OR. (ID2.EQ.1)) ISUBAX(2,I) = 1
         IF ((DTYPAX(1,I).NE.3) .AND. (DTYPAX(1,I).NE.5) .AND.
     *      (DTYPAX(1,I).NE.7) .AND. (DTYPAX(2,I).NE.3) .AND.
     *      (DTYPAX(2,I).NE.5) .AND. (DTYPAX(2,I).NE.7)) GO TO 55
            WRITE (MSGTXT,1050) CHAX(I)
            GO TO 990
 55      IF ((ISUBAX(1,I).LE.ID1) .AND. (ISUBAX(2,I).LE.ID2)) GO TO 60
            WRITE (MSGTXT,1055) CHAX(I), ISUBAX(1,I), ISUBAX(2,I),
     *         ID1, ID2
            GO TO 990
 60      CONTINUE
      IF (BPARM(1).GT.0.) ABSEM(1,2) = .FALSE.
      IF (BPARM(1).GT.0.) ABSEM(2,2) = .FALSE.
      IF (BPARM(1).GT.0.) OPTYPE(2) = 0
      IF (BPARM(3).LT.1.0E-20) BPARM(3) = 1.0E-20
      IF (OPTYPE(1).GT.19) OPTYPE(1) = 0
      IF (OPTYPE(2).GT.19) OPTYPE(2) = 0
      APARM(5) = OPTYPE(1)
      APARM(10) = OPTYPE(2)
      IF (BPARM(9).EQ.0.0) BPARM(9) = 1.0
      IF (BPARM(10).EQ.0.0) BPARM(10) = 1.0
      IF (CPARM(1).EQ.0.0) CPARM(1) = 1.0
      IF (CPARM(3).EQ.0.0) CPARM(3) = 1.0
      IF (CPARM(5).EQ.0.0) CPARM(5) = 1.0
      IF (CPARM(7).EQ.0.0) CPARM(7) = 1.0
      IF (CPARM(9).EQ.0.0) CPARM(9) = 1.0
C                                       parms
      APARM(2) = ISUBAX(1,1)
      APARM(4) = ISUBAX(2,1)
      APARM(7) = ISUBAX(1,2)
      APARM(9) = ISUBAX(2,2)
      CALL TABIO ('CLOS', 0, IRNO, SBUF, TABUF, IERR)
C                                       Create output file?
      IF (OVER.EQ.IVER) GO TO 100
         CALL ZPHFIL (INEXT, DISKIN, CNO, IVER, PFILE, IERR)
         CALL ZPHFIL (INEXT, DISKIN, CNO, OVER, OPFILE, IERR)
         CALL ZEXIST (DISKIN, OPFILE, OISIZE, IERR)
         IF (IERR.LE.1) GO TO 65
            WRITE (MSGTXT,1060) IERR
            GO TO 990
 65      IF (IERR.EQ.1) GO TO 70
            CALL ZDESTR (DISKIN, OPFILE, IERR)
            IF (IERR.LE.1) GO TO 70
               WRITE (MSGTXT,1065) IERR
               GO TO 990
 70      ISIZE = INREC
         CALL ZCREAT (DISKIN, OPFILE, ISIZE, F, OISIZE, IERR)
         IF (IERR.EQ.0) GO TO 75
            WRITE (MSGTXT,1070) IERR
            GO TO 990
 75      CALL ZOPEN (T2LUN, T2IND, DISKIN, PFILE, F, F, T, IERR)
         IF (IERR.EQ.0) GO TO 80
            WRITE (MSGTXT,1075) IERR
            GO TO 980
 80      CALL ZOPEN (TALUN, TAIND, DISKIN, OPFILE, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 85
            WRITE (MSGTXT,1080) IERR
            GO TO 980
 85      DO 90 IRNO = 1,INREC
            CALL ZFIO ('READ', T2LUN, T2IND, IRNO, TABUF, IERR)
            IF (IERR.EQ.0) CALL ZFIO ('WRIT', TALUN, TAIND, IRNO, TABUF,
     *         IERR)
            IF (IERR.EQ.0) GO TO 90
               WRITE (MSGTXT,1085) IERR
               GO TO 970
 90         CONTINUE
         CALL ZCLOSE (TALUN, TAIND, IERR)
         CALL ZCLOSE (T2LUN, T2IND, IERR)
C                                       update header
C                                       FXHDEX called by FNDEXT, TABINI
         IF (IMAXV.GE.OVER) GO TO 100
            DO 95 I = 1,KIEXTN
               CALL H2CHR (2, 1, CATH(KHEXT+I-1), CHTM12)
               EQUAL = INEXT.EQ. CHTM12(1:2)
               IF (EQUAL) GO TO 96
 95            CONTINUE
            GO TO 100
 96         CONTINUE
               CATBLK(KIVER+I-1) = OVER
C                                       Open the output table
 100  CALL TABINI ('WRIT', INEXT, DISKIN, CNO, OVER, CATBLK, TALUN,
     *   NKEY, NREC, NCOL, PBUF, TABUF, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1030) IERR, INEXT, OVER
         GO TO 980
 110  TAIND = TABUF(82)
      GO TO 999
C                                       Error message: delete
 970  CALL MSGWRT (8)
      CALL ZCLOSE (TALUN, TAIND, IERR)
      CALL ZDESTR (DISKIN, OPFILE, IERR)
      JERR = 6
      GO TO 999
C                                       Error message: delete
 980  CALL MSGWRT (8)
      CALL ZDESTR (DISKIN, OPFILE, IERR)
      JERR = 6
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (8)
      JERR = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('COMPARISON OPERATION ''',A4,''' NOT RECOGNIZED')
 1020 FORMAT ('ERROR',I4,' FINDING ',A12,'.',A6,'.',I4,' DISK',I2,
     *   ' USER',I5)
 1025 FORMAT ('ERROR',I4,' READING CATALOG HEADER')
 1030 FORMAT ('ERROR',I4,' OPENING EXT. ',A2,' VERS',I6)
 1040 FORMAT ('ERROR IN BCOUNT, ECOUNT:',2I8)
 1045 FORMAT (A2,'AXIS COLUMN NUMBERS',2I5,' OUT OF RANGE')
 1050 FORMAT ('I FLAG ONLY FLOATING OR INTEGER COLUMNS: PROBLEM IN ',A2,
     *   'AXIS')
 1055 FORMAT (A2,'AXIS SUBSCRIPTS',2I6,' EXCEED LIMITS',2I6)
 1060 FORMAT ('ZEXIST ERROR',I5,' CHECKING OUTPUT FILE')
 1065 FORMAT ('ZDESTR ERROR',I5,' DESTROYING OLD OUTPUT FILE')
 1070 FORMAT ('ZCREAT ERROR',I5,' CREATING OUTPUT FILE')
 1075 FORMAT ('ERROR',I5,' OPENING INPUT TABLE FILE')
 1080 FORMAT ('ERROR',I5,' OPENING OUTPUT TABLE FILE')
 1085 FORMAT ('ERROR',I5,' READING OR WRITING TABLE FILE')
      END
      SUBROUTINE TAFLAG (IRET)
C-----------------------------------------------------------------------
C   TAFLAG actually flags table data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER COLTYP(2)*8, OP*4, TTXT*1024, CTEMP1*4, CTEMP2*4
      HOLLERITH HTXT(256)
      INTEGER   IERR, I, INCHAR, J, IP, ITTXT(20), IROW, LBCNT, J4, I4,
     *   NFL, IFL(7)
      REAL      DUMFAC(2)
      DOUBLE PRECISION    XZY(2)
      LOGICAL   EQUAL
      INCLUDE 'TAFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ITTXT, HTXT)
      DATA COLTYP /2*' '/
C-----------------------------------------------------------------------
      NCOUNT = 0
      IRET = 1
      RSUM(1) = 0.0D0
      RSUM(2) = 0.0D0
C                                       Loop
      LBCNT = IBCNT
      IF ((SUMEM(1)) .OR. (SUMEM(2))) LBCNT = 1
      NFL = 0
      DO 80 IROW = LBCNT,IECNT
C                                       get RECI for GETCOL in FNDTXY
         OP = 'READ'
         CALL TABIO (OP, 0, IROW, RECI, TABUF, IRET)
         IF (IRET.GT.0) GO TO 960
         CALL FNDTXY (IROW, XZY, IRET)
         IF (IRET.GT.0) GO TO 960
         IF (IRET.LT.-1) GO TO 80
         IF (IROW.LT.IBCNT) GO TO 80
         IF ((IRET.LT.0) .AND. (BPARM(4).LE.0.0)) GO TO 80
         IF ((IRET.EQ.0) .AND. (BPARM(4).GT.0.0)) GO TO 80
C                                       Do comparison
         GO TO (10, 20, 30, 40, 50, 60), IOP
 10      CONTINUE
            IF (XZY(1).GT.XZY(2)) GO TO 70
            GO TO 80
 20      CONTINUE
            IF (XZY(1).GE.XZY(2)) GO TO 70
            GO TO 80
 30      CONTINUE
            IF (ABS(XZY(1)-XZY(2)).LT.BPARM(3)) GO TO 70
            GO TO 80
 40      CONTINUE
            IF (ABS(XZY(1)-XZY(2)).GE.BPARM(3)) GO TO 70
            GO TO 80
 50      CONTINUE
            IF (XZY(1).LE.XZY(2)) GO TO 70
            GO TO 80
 60      CONTINUE
            IF (XZY(1).LT.XZY(2)) GO TO 70
            GO TO 80
C                                       Comparison was true
 70      CONTINUE
            NFL = NFL + 1
            IFL(NFL) = IROW
            IF (BPARM(4).GT.0.0) OP = 'WRIT'
            IF (BPARM(4).LE.0.0) OP = 'FLAG'
            CALL TABIO (OP, 0, IROW, RECI, TABUF, IRET)
            IF (IRET.NE.0) GO TO 960
            NCOUNT = NCOUNT + 1
            IF (NFL.EQ.7) THEN
               IF (BPARM(4).GT.0.0) THEN
                  WRITE(MSGTXT,1070) IFL
               ELSE
                  WRITE(MSGTXT,1071) IFL
                  END IF
               CALL MSGWRT (2)
               NFL = 0
               END IF
 80      CONTINUE
      IF (NFL.GT.0) THEN
         IF (BPARM(4).GT.0.0) THEN
            WRITE(MSGTXT,1070) (IFL(IP), IP = 1,NFL)
         ELSE
            WRITE(MSGTXT,1071) (IFL(IP), IP = 1,NFL)
            END IF
         CALL MSGWRT (2)
         END IF
C                                       Build reason string
      IP = 1
      TEXT = ' '
C                                       Row range
      WRITE (TTXT,1080) IBCNT
      CALL CHTRIM (TTXT, 12, TTXT, INCHAR)
      TEXT(IP:IP+INCHAR-1) = TTXT(1:INCHAR)
      IP = IP + INCHAR
      WRITE (TTXT,1080) IECNT
      CALL CHTRIM (TTXT, 12, TTXT, INCHAR)
      INCHAR = INCHAR - 1
      TEXT(IP:IP+INCHAR-1) = TTXT(1:INCHAR)
      IP = IP + INCHAR + 1
C                                       first column(s)
      CALL SELSTR (IP, OPTYPE(1), TWOOF(1), ABSEM(1,1), TYPEAX(1,1),
     *   ISUBAX(1,1), CPARM(1), CPARM(9), BPARM(9), COLTYP, IP, TEXT)
C                                       comparison operator
      IP = IP + 1
      TEXT(IP:IP+3) = XPTYPE
      IP = IP + 3
      CALL CHTRIM (TEXT, IP, TEXT, INCHAR)
      IP = INCHAR + 2
C                                       2nd columns
      IF (BPARM(1).GT.0.0) GO TO 90
         DUMFAC(1) = 1.0
         DUMFAC(2) = 0.0
         CALL SELSTR (IP, OPTYPE(2), TWOOF(2), ABSEM(1,2), TYPEAX(1,2),
     *      ISUBAX(1,2), CPARM(5), DUMFAC, BPARM(10), COLTYP, IP, TEXT)
         GO TO 100
C                                       constant comparand
 90   CONTINUE
         WRITE (TTXT,1090) BPARM(2)
         CALL CHTRIM (TTXT, 12, TTXT, INCHAR)
         TEXT(IP:IP+INCHAR-1) = TTXT(1:INCHAR)
         IP = IP + INCHAR
C                                       And UNflag
 100  INCHAR = IP - 1
      IF (BPARM(4).LE.0.0) GO TO 130
         TEXT(IP:IP+5) = 'UNFLAG'
         IP = IP + 6
 130  IP = IP - 1
      TTXT(1:IP) = TEXT(1:IP)
      I = (IP-1) / 4 + 2
      CALL CHR2H (IP, TTXT, 1, HTXT)
      ITTXT(I) = 0
      ITTXT(I+1) = 0
C                                       reason string: clear on unflag
      LBCNT = TABUF(61)
      IF ((LBCNT.LE.0) .OR. (BPARM(4).LE.0.0)) GO TO 170
         DO 140 IROW = 1,LBCNT
            OP = 'READ'
            CALL TABIO (OP, 2, IROW, RECI, TABUF, IRET)
            IF (IRET.GT.0) GO TO 960
            J = TABUF(IROW+1) - TABUF(IROW)
            IF (IROW.EQ.LBCNT) J = TABUF(62) - TABUF(IROW)
            DO 135 I = 1,J
               CALL H2CHR (4, 1, HTXT(I), CTEMP1)
               CALL H2CHR (4, 1, RECR(I), CTEMP2)
               EQUAL = CTEMP1 .EQ. CTEMP2
               IF (.NOT.EQUAL) GO TO 140
 135           CONTINUE
            GO TO 150
 140        CONTINUE
         GO TO 170
C                                       remove old selection string
 150     CONTINUE
            J4 = IROW + 1
            IF (IROW.GE.LBCNT) GO TO 160
            DO 155 I4 = J4,LBCNT
               IROW = I4
               OP = 'READ'
               CALL TABIO (OP, 2, IROW, RECI, TABUF, IRET)
               IF (IRET.GT.0) GO TO 960
               TABUF(62+IROW) = TABUF(62+IROW-1) + TABUF(62+IROW+1)
     *            - TABUF(62+IROW)
               IROW = IROW - 1
               OP = 'WRIT'
               CALL TABIO (OP, 2, IROW, RECI, TABUF, IRET)
               IF (IRET.GT.0) GO TO 960
 155           CONTINUE
 160        TABUF(62) = TABUF(62+LBCNT)
            TABUF(61) = LBCNT - 1
            GO TO 180
C                                       Just write selection string
 170  IROW = TABUF(61) + 1
      IF (IROW.LE.8) THEN
         OP = 'WRIT'
         CALL TABIO (OP, 2, IROW, HTXT, TABUF, IRET)
         IF (IRET.GT.0) GO TO 960
         IF (TABUF(61).LT.IROW) TABUF(61) = IROW
      ELSE
         MSGTXT = 'Selection string area full: reason not recorded'
         CALL MSGWRT (6)
         MSGTXT = 'This is probably unimportant'
         CALL MSGWRT (6)
         END IF
C                                       Close the table
 180  CALL TABIO ('CLOS', 2, IROW, RECI, TABUF, IRET)
      IF (IRET.EQ.0) GO TO 990
C                                       Destroy the output file
 960  WRITE (MSGTXT,1960) IRET, OP, IROW
      CALL MSGWRT (8)
      CALL ZCLOSE (TALUN, TAIND, IERR)
      CALL ZDESTR (DISKIN, OPFILE, IERR)
      GO TO 999
C                                       Messages
 990  WRITE (MSGTXT,1990) NCOUNT
      IF (BPARM(4).GT.0.0) WRITE (MSGTXT,1991) NCOUNT
      CALL MSGWRT (2)
      WRITE (MSGTXT,1992) INEXT, OVER
      CALL MSGWRT (2)
      IRET = 0
C                                       Update catalog header
      CALL CATIO ('UPDT', DISKIN, CNO, CATBLK, 'REST', SBUF, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1995) IERR
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('Unflagged rows',7I7)
 1071 FORMAT ('Flagged rows',7I7)
 1080 FORMAT (I11,':')
 1090 FORMAT (1PE12.5)
 1960 FORMAT ('ERROR',I5,1X,A4,'ING ROW',I6)
 1990 FORMAT ('TAFLAG: ',I10,' rows flagged')
 1991 FORMAT ('TAFLAG: ',I10,' rows unflagged')
 1992 FORMAT ('TAFLAG: ',A2,' file version',I5,'  written.')
 1995 FORMAT ('ERROR',I5,' UPDATING THE CATALOG HEADER')
      END
      SUBROUTINE FNDTXY (IROW, XY, IRET)
C-----------------------------------------------------------------------
C   FNDTXY extracts the desired X and Y values from the table file.
C   Inputs:
C      IROW   I      Table row number - already read in RECI
C   Outputs:
C      XY     D(2)   X, Y values
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   IROW, IRET
      DOUBLE PRECISION XY(2)
C
      INCLUDE 'TAFLG.INC'
      DOUBLE PRECISION DADATA(MAXCIF/2), XZY(2)
      REAL      D4DATA(MAXCIF)
      INTEGER   D3DATA(MAXCIF), I, D2DATA(MAXCIF), J, JT, IO, LROW
      LOGICAL   WASFLG
      EQUIVALENCE (DADATA, D4DATA, D3DATA, D2DATA)
C-----------------------------------------------------------------------
      WASFLG = .FALSE.
      LROW = IROW
C                                       Loop over axes
      DO 100 I = 1,2
         XY(I) = BPARM(2)
         IF ((I.EQ.2) .AND. (BPARM(1).GT.0.0)) GO TO 100
C                                       Get values
         DO 10 J = 1,2
            IF ((J.EQ.2) .AND. (.NOT.TWOOF(I))) GO TO 10
            XZY(J) = IROW
            IF (TYPEAX(J,I).EQ.0) GO TO 5
            CALL GETCOL (IROW, TYPEAX(J,I), PBUF, LROW, TABUF, JT,
     *         DADATA, RECI, IRET)
            JT = MOD (JT, 10)
            IF (IRET.GT.0) GO TO 999
            IF (IRET.LT.0) WASFLG = .TRUE.
            IF (JT.EQ.1) XZY(J) = DADATA(ISUBAX(J,I))
            IF (JT.EQ.2) XZY(J) = D4DATA(ISUBAX(J,I))
            IF (JT.EQ.4) XZY(J) = D3DATA(ISUBAX(J,I))
            IF (JT.EQ.6) XZY(J) = D2DATA(ISUBAX(J,I))
            IF (ABSEM(J,I)) XZY(J) = ABS(XZY(J))
 5          IO = 4 * I + 2 * J - 5
            XZY(J) = XZY(J) * CPARM(IO) + CPARM(IO+1)
 10         CONTINUE
C                                       perform function
         IRET = -2
         J = OPTYPE(I) + 1
         GO TO (20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
     *      34, 35, 36, 37, 38, 39), J
C                                       straight through
 20      XY(I) = XZY(1)
         GO TO 85
C                                       Sum
 21      IF (.NOT.WASFLG) RSUM(I) = RSUM(I) + XZY(1)
         XY(I) = RSUM(I)
         GO TO 85
C                                       ASIN
 22      IF (ABS(XZY(1)).GT.1.0D0) GO TO 999
         XY(I) = ASIN (XZY(1)) / DEG2R
         GO TO 85
C                                       LOG10
 23      IF (XZY(1).LE.0.0D0) GO TO 999
         XY(I) = LOG10 (XZY(1))
         GO TO 85
C                                       LN
 24      IF (XZY(1).LE.0.0D0) GO TO 999
         XY(I) = LOG (XZY(1))
         GO TO 85
C                                       EXP
 25      XY(I) = EXP (XZY(1))
         GO TO 85
C                                       SIN
 26      XY(I) = SIN (DEG2R * XZY(1))
         GO TO 85
C                                       COS
 27      XY(I) = COS (DEG2R * XZY(1))
         GO TO 85
C                                       TAN
 28      XY(I) = TAN (DEG2R * XZY(1))
         GO TO 85
C                                       ARC TAN
 29      XY(I) = ATAN (XZY(1)) / DEG2R
         GO TO 85
C                                       +
 30      XY(I) = XZY(1) + XZY(2)
         GO TO 85
C                                       -
 31      XY(I) = XZY(1) - XZY(2)
         GO TO 85
C                                       *
 32      XY(I) = XZY(1) * XZY(2)
         GO TO 85
C                                       /
 33      IF (XZY(2).EQ.0.D0) GO TO 999
         XY(I) = XZY(1) / XZY(2)
         GO TO 85
C                                       **
 34      IF (XZY(1).LT.0.D0) GO TO 999
         XY(I) = XZY(1) ** XZY(2)
         GO TO 85
C                                       MOD
 35      IF (XZY(2).EQ.0.D0) GO TO 999
         XY(I) = MOD (XZY(1), XZY(2))
         GO TO 85
C                                       Modulus
 36      XY(I) = SQRT (XZY(1)*XZY(1) + XZY(2)*XZY(2))
         GO TO 85
C                                       ATAN2
 37      IF ((XZY(1).EQ.0.D0) .AND. (XZY(2).EQ.0.D0)) GO TO 999
         XY(I) = ATAN2 (XZY(1), XZY(2))
         GO TO 85
C                                       Max
 38      XY(I) = XZY(1)
         IF (XZY(2).GT.XZY(1)) XY(I) = XZY(2)
         GO TO 85
C                                       Min
 39      XY(I) = XZY(1)
         IF (XZY(2).LT.XZY(1)) XY(I) = XZY(2)
C                                       Okay!: scale
 85      IO = 8 + I
         IF (BPARM(IO).EQ.1.0) GO TO 95
         IF (BPARM(IO).EQ.0.0) GO TO 95
            IF ((XY(I).EQ.0.0D0) .AND. (BPARM(IO).LT.1.0)) GO TO 999
            JT = BPARM(IO)
            IF (JT.EQ.BPARM(IO)) GO TO 90
               IF (XY(I).LT.0.0D0) GO TO 999
               XY(I) = XY(I) ** BPARM(IO)
               GO TO 95
 90         CONTINUE
               XY(I) = XY(I) ** JT
 95      IF (I.EQ.1) XY(I) = XY(I) * CPARM(9) + CPARM(10)
 100     CONTINUE
      IRET = 0
      IF (WASFLG) IRET = -1
C
 999  RETURN
      END
      SUBROUTINE TAFLHI
C-----------------------------------------------------------------------
C   TAFLHI adds remarks to the history file of TAFLG
C-----------------------------------------------------------------------
      CHARACTER LINE*72, ATIME*8, ADATE*12
      INTEGER   IHLUN, IERR, IT(3), ID(3), I, IP, LP
      LOGICAL   T
      INCLUDE 'TAFLG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIINIT (3)
      IHLUN = 29
      CALL HIOPEN (IHLUN, DISKIN, CNO, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       table type, version numbers
 10   CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (LINE,1010) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (IHLUN, LINE, SBUF, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (LINE,1011) TSKNAM, INEXT, IVER, OVER
      CALL HIADD (IHLUN, LINE, SBUF, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (LINE,1012) TSKNAM, IBCNT, IECNT
      CALL HIADD (IHLUN, LINE, SBUF, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (LINE,1013) TSKNAM, BPARM(3)
      IF ((IOP.EQ.3) .OR. (IOP.EQ.4)) THEN
         CALL HIADD (IHLUN, LINE, SBUF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 20
      DO 14 IP = 1,1024,64
         LINE(7:) = ' '
         LP = 1025 - IP
         CALL CHBLNK (LP, IP, TEXT, I)
         IF (I.LE.0) GO TO 15
            LINE(7:72) = TEXT(IP:IP+63)
            CALL HIADD (IHLUN, LINE, SBUF, IERR)
            IF (IERR.NE.0) GO TO 20
 14      CONTINUE
 15   IF (BPARM(4).LE.0.0) WRITE (LINE,1015) TSKNAM, NCOUNT
      IF (BPARM(4).GT.0.0) WRITE (LINE,1016) TSKNAM, NCOUNT
      CALL HIADD (IHLUN, LINE, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 30
C                                       error
 20   WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (6)
C                                       close it
 30   CALL HICLOS (IHLUN, T, SBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'Release =''',A7,' ''  /********* start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,' INEXT=''',A2,''' INVER=',I3,' OUTVER=',I3)
 1012 FORMAT (A6,' BCOUNT=',I10,' ECOUNT=',I12)
 1013 FORMAT (A6,' TOLERANCE=',1PE13.6)
 1015 FORMAT (A6,' / flagged',I9,' rows')
 1016 FORMAT (A6,' / unflagged',I9,' rows')
 1020 FORMAT ('ERROR',I5,' WRITING HISTORY FILE')
      END
