LOCAL INCLUDE 'VLOG.INC'
C                                       Include VLOG
C                                       Local include for VLOG;
C                                       requires PUVD.INC
      INTEGER MXFQID
C                                       Max. no. of FQ IDs
      PARAMETER (MXFQID = 12)
C                                       Input parameters
      HOLLERITH XNAMEI(3), XCLAIN(2), XINFIL(12), XOUTFL(12)
      CHARACTER LNAME*12, LCLASS*6, LINFIL*48, LOUTFL*48
      REAL      XINSEQ, XINDIS, XSUBA, XFQTOL, XPRTLV, FQTOL
      INTEGER   INSEQ, INDISK, ISUBA, IPRTLV
C
      COMMON /INPARM/ XNAMEI, XCLAIN, XINSEQ, XINDIS, XSUBA, XINFIL,
     *   XOUTFL, XFQTOL, XPRTLV
      COMMON /INVAL/ FQTOL, INSEQ, INDISK, ISUBA, IPRTLV
      COMMON /CHVAL/ LNAME, LCLASS, LINFIL, LOUTFL
C                                       Buffers
      INTEGER   JBUFF1(512), JBUFF2(512)
      COMMON /WRKBUF/ JBUFF1, JBUFF2
C                                       General global variables
      DOUBLE PRECISION DREFYR(2)
      INTEGER   IFQUV(MXFQID), NFQUV, ILUNF, ILUNTB, ICNO, IFINDF,
     *   ILUNO, IFINDO, IPOLUV, NPOLUV, IREFDY
      COMMON /GENPAR/ DREFYR, IFQUV, NFQUV, ILUNF, ILUNTB, ICNO,
     *   IFINDF, ILUNO, IFINDO, IPOLUV, NPOLUV, IREFDY
C                                       FQ information
      DOUBLE PRECISION DFRQTB(MXFQID,MAXIF,2)
      INTEGER   NIFTB
      COMMON /FQCOM/ DFRQTB, NIFTB
C                                       NX table information
      INTEGER   MAXNX
      PARAMETER (MAXNX = 4096)
      REAL      TIMENX(2,MAXNX)
      INTEGER   INXSOU(MAXNX), INXFQ(MAXNX), NXDAT
      COMMON /NXCOM/ TIMENX, INXSOU, INXFQ, NXDAT
LOCAL END
      PROGRAM VLOG
C----------------------------------------------------------------------
C! Pre-process external VLBI calibration files
C# UV Calibration VLB
C----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2006-2007, 2012, 2014-2017
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
C   Task VLOG pre-processes external VLBI calibration information
C   into the format expected by AIPS
C   Inputs:
C      AIPS adverb    Local var.    Description
C      INNAME         LNAME         Input uv-file name
C      INCLASS        LCLASS        Input uv-file class
C      INSEQ          INSEQ         Input uv-file seq. number
C      INDISK         INDISK        Disk no. of input uv-file
C      SUBARRAY       ISUBA         Subarray number
C      INFILE         LINFIL        Ext. calibration file name
C      OUTFILE        LOUTFL        Prefix for output files
C      PRTLEV         IPRTLV        Print/debug level
C----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      CHARACTER LPGM*6
      INTEGER IRET
      DATA LPGM /'VLOG  '/
C----------------------------------------------------------------------
C                                       Get input parameters and
C                                       perform general initialization
      CALL VLOGIN (LPGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Process the external
C                                       calibration information
      CALL VLCAL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update history file
      CALL VLOGHI (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files/exit
990   CALL DIE (IRET, JBUFF1)
C
999   STOP
      END
      SUBROUTINE VLOGIN (LPGM, IRET)
C----------------------------------------------------------------------
C   Read input parameters for VLOG and perform general initialization
C   Inputs:
C      LPGM       C*6        Task name
C   Outputs:
C      IRET       I          Return code (0 => ok)
C----------------------------------------------------------------------
      CHARACTER LPGM*6
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTRUE
      CHARACTER LSTAT*4, LTYPE*2, LDATE*8
      INTEGER NPARM, IERR, IDATE(3), ILOYR
      DATA WTRUE /.TRUE./
C----------------------------------------------------------------------
      IRET = 0
C                                       LUN for input and output
C                                       external text files
      ILUNF = 10
      ILUNO = 11
C                                       General LUN for table I/O
      ILUNTB = 27
C                                       Initialise AIPS from disk
      CALL ZDCHIN (WTRUE)
C                                       Compute pointers for
C                                       catalog records
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input adverbs
      NPARM = 34
      CALL GTPARM (LPGM, NPARM, RQUICK, XNAMEI, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = WTRUE
         IRET = 1
C                                       Check if initiator (AIPS)
C                                       not found
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, JBUFF1, IERR)
      IF (IERR.NE.0) IRET = 1
C                                       Abort if error obtaining
C                                       input parameters
      IF (IRET.NE.0) GO TO 999
C                                       Convert input parameters
      CALL H2CHR (12, 1, XNAMEI, LNAME)
      CALL H2CHR (6, 1, XCLAIN, LCLASS)
      CALL H2CHR (48, 1, XINFIL, LINFIL)
      CALL H2CHR (48, 1, XOUTFL, LOUTFL)
C
      INSEQ = XINSEQ
      INDISK = XINDIS
      ISUBA = XSUBA
      ISUBA = MAX (ISUBA, 1)
C                                       Tolerance for IF match (kHz).
C                                       Convert to MHz
      FQTOL = MAX (XFQTOL, 1.0E-4) / 1000.0
      IPRTLV = XPRTLV
C                                       Find uv file in catalog dir
      LSTAT = 'SRCH'
      LTYPE = 'UV'
      ICNO = 1
      CALL CATDIR ('SRCH', INDISK, ICNO, LNAME, LCLASS, INSEQ, LTYPE,
     *   NLUSER, LSTAT, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 2
         GO TO 990
         END IF
C                                       Read catalog header; mark file
C                                       status as 'READ'
      CALL CATIO ('READ', INDISK, ICNO, CATBLK, 'WRIT', JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 3
         GO TO 990
         END IF
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = ICNO
      FRW(NCFILE) = 1
C                                       Get uv-header information
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         IRET = 4
         GO TO 990
         END IF
C                                       Extract Stokes axis parameters
      IF (JLOCS.GT.0) THEN
         NPOLUV = CATBLK(KINAX+JLOCS)
      ELSE
         NPOLUV = 1
         END IF
      IPOLUV = ICOR0
C                                       Read NX table
      CALL NXREAD (IRET)
      IF (IERR.NE.0) THEN
         IRET = 5
         GO TO 999
         END IF
C                                       Read FQ table
      CALL FQREAD (IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         GO TO 999
         END IF
C                                       Read AN table information
      CALL GETANT (INDISK, ICNO, ISUBA, CATBLK, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 7
         GO TO 999
         END IF
C                                       Extract ref. day number
C                                       from catalog header
      CALL H2CHR (8, 1, CATH(KHDOB), LDATE)
      CALL DATEST (LDATE, IDATE)
      CALL DAYYR (IDATE(1), IDATE(2), IDATE(3), IREFDY, ILOYR)
C                                       Should not use *cal.vlba calibration
C                                       files on RDBE data.  All obs after
C                                       Jan 31 2014 are RDBE
      IF ((IDATE(1).EQ.14.AND.IDATE(2).GT.1).OR.(IDATE(1).EQ.15.AND.
     *  IDATE(2).LT.11)) THEN
          WRITE(MSGTXT,1070)
          CALL MSGWRT(8)
          WRITE(MSGTXT,1080)
          CALL MSGWRT(8)
          WRITE(MSGTXT,1090)
          CALL MSGWRT(8)
          WRITE(MSGTXT,1100)
          CALL MSGWRT(8)
          WRITE(MSGTXT,1110)
          CALL MSGWRT(8)
          ENDIF
C                                       Min., max. times in the
C                                       uv-data
      DREFYR(1) = (IREFDY + TIMENX(1,1)) / DBLE (ILOYR) + IDATE(1)
      DREFYR(2) = (IREFDY + TIMENX(2,NXDAT)) / DBLE (ILOYR) + IDATE(1)
      GO TO 999
C                                       Error
990   CALL MSGWRT (6)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('VLOGIN: ERR',I3,' OBTAINING INPUT PARAMETERS')
1020  FORMAT ('VLOGIN: ERR',I3,' FINDING ',A12,'.',A6,'.',I4,'.',I3)
1040  FORMAT ('VLOGIN: ERR',I3,' READING HEADER ',A12,'.',A6,'.',I4,
     *   '.',I3)
1060  FORMAT ('VLOGIN: ERR',I3,' DECODING HEADER')
1070  FORMAT ('! WARNING: If you are using the *cal.vlba* file,
     * please be')
1080  FORMAT ('! aware that this data is from the VLBA LEGACY
     * system.')
1090  FORMAT ('! The LEGACY system has a signal path that is
     * different')
1100  FORMAT ('! from the RDBE/MARK5C system.  Do not use *cal.vlba*
     * file ')
1110  FORMAT ('! unless you understand what you are doing.')
      END
      SUBROUTINE VLCAL (IRET)
C----------------------------------------------------------------------
C   Subroutine to pre-process the external calibration information
C   Outputs:
C      IRET      I        Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       16 colums max. in TSM format
      INTEGER MAXCOL
      PARAMETER (MAXCOL = 16)
C                                       14 frequency codes at present
      INTEGER MXCODE
      PARAMETER (MXCODE = 14)
      LOGICAL WEOF, WERROR, WFIRST, WTRUE, WFALSE, WSKIP, WSAVE,
     *   WBAD, WFRST1, WFRST2, WANTD
      CHARACTER LSTATE*8, LINE*132, LSTATN*8, LFILE*99, LMATCH*132,
     *   LOUT*132, LSAVE(3)*132, LTMP7*7, LPOL(MAXCOL), LAST*8,
     *   LANT*2, LTSYS*132, LFCODE(MXCODE)*8
      DOUBLE PRECISION DFQIF(MAXCOL,2), DFRQLO(MXCODE), DFRQHI(MXCODE),
     *   DBW, DCENT, DFGHZ, DYEAR1, DYEAR2
      INTEGER IIF(MAXCOL), NCOL, IERR, N1, N2, J, IFND, JERR, NSAVE,
     *   NFILL, ICOL, JANT, IDAY1, IDAY2, JCODE, K, M, IYR1, IMNTH1,
     *   IHR1, IYR2, IMNTH2, IHR2, ILOYR1, ILOYR2, IDAYN1, IDAYN2,
     *   JT, JTRIM
C                                       VLBA frequency codes used in
C                                       vlba_gains.key (1/97)
      DATA LFCODE /'90cm', '50cm', '20cm', '13cm', '13cmsx', '6cm',
     *   '4cm', '4cmsx', '2cm', '1cm', '7mm', '21cm', '18cm', '3mm' /
C                                       Nominal freq. ranges (GHz)
      DATA DFRQLO /0.302, 0.588, 1.18, 1.92, 1.92, 3.9, 7.9, 7.9,
     *   11.0, 20.5, 37.6, 1.18, 1.10, 79.712/
      DATA DFRQHI /0.352, 0.633, 1.85, 2.84, 2.84, 7.899, 9.05,9.05,
     *   16.0, 25.3, 46.2, 1.85, 1.85, 96.024 /
      DATA WTRUE /.TRUE./, WFALSE /.FALSE./
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Markers indicating beginning
C                                       of section/ sub-section
      WFRST1 = WFALSE
      WFRST2 = WFALSE
C                                       Open calibration log file
      CALL ZTXOPN ('READ', ILUNF, IFINDF, LINFIL, WFALSE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, LINFIL
         IRET = 9
         GO TO 990
         END IF
C                                       Read state, line
      LSTATN = 'NULL'
      LINE = ' '
      WFIRST = WTRUE
C                                       Read first line
100   CALL ZTXIO ('READ', ILUNF, IFINDF, LINE, IERR)
      JT = JTRIM (LINE)
      CALL LINCHK (LINE)
C                                       Echo if print level set
      IF (IPRTLV.GT.1) THEN
         MSGTXT = LINE(1:64)
         CALL MSGWRT (6)
         END IF
C
      WEOF = (IERR.EQ.2)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
C                                       While (NOT EOF or ERROR) do:
      IF (WERROR) GO TO 900
C                                       Default is to copy input line
C                                       to output file
         WSKIP = WFALSE
C                                       Default is not to print lines
C                                       currently accumulated in stack
         WSAVE = WFALSE
C                                       Check for new state:
C                                       (Tsys, Flag, Wx, Gain, Pcal,
C                                        Scan, MkIII)
         LSTATE = LSTATN
         IF (.NOT.WEOF) CALL STATE (LINE, LSTATN)
C                                       Terminate preceding WX
C                                       sub-section if necessary
         IF ((LSTATE.EQ.'WX').AND.(WEOF.OR.(LSTATN.NE.LSTATE))) THEN
            LOUT = '/'
            CALL ZTXIO ('WRIT', ILUNO, IFINDO, LOUT(1:1), IERR)
            IF (IERR.NE.0) THEN
               IRET = 8
               WRITE (MSGTXT,1800) IERR, LFILE
               GO TO 990
               END IF
            END IF
C                                       Check for EOF
         IF (WEOF) GO TO 900
C                                       Ignore blank lines
         CALL CHBLNK (132, 1, LINE, N1)
         IF (N1.LE.0) GO TO 100
C                                       If state has changed then
C                                       change current output file
         IF ((LSTATE.NE.LSTATN).AND.(LSTATN.NE.'NULL')) THEN
C                                       Construct new file name
            N1 = JTRIM (LOUTFL)
            N2 = JTRIM (LSTATN)
            IF ((N1+N2).LE.99) THEN
C                                       Note: add gain information
C                                       to TSYS file
               IF (LSTATN.EQ.'GAIN') THEN
                  LFILE = LOUTFL(1:N1) // '.TSYS'
               ELSE
                  LFILE = LOUTFL(1:N1) // '.' // LSTATN(1:N2)
                  END IF
            ELSE
               IRET = 1
               WRITE (MSGTXT,1150)
               GO TO 990
               END IF
C                                       Close current output file
            IF (.NOT.WFIRST) THEN
               CALL ZTXCLS (ILUNO, IFINDO, IERR)
               END IF
C                                       Open new output file for append
            CALL ZTXOPN ('WRIT', ILUNO, IFINDO, LFILE, WTRUE, IERR)
            IF (IERR.NE.0) THEN
               IRET = 2
               WRITE (MSGTXT,1100) IERR, LFILE
               GO TO 990
               END IF
C                                       Start of new section
            WFRST1 = WTRUE
C                                       No longer first file
            WFIRST = WFALSE
C                                       Last line type read
            LAST = 'NULL'
            END IF
C                                       Preprocess the new line.
         LOUT = LINE
C                                       Case state of:
C======================================================================
C                                       1: 'TSYS'
         IF (LSTATN.EQ.'TSYS') THEN
C                                       Case line type of:
C----------------------------------------------------------------------
C                                       a: Section header
            IF (LINE(1:24).EQ.'! ----- Tsys information') THEN
               WFRST2 = WTRUE
C----------------------------------------------------------------------
C                                       b: TSYS line
            ELSE IF (LINE(1:4).EQ.'TSYS') THEN
C                                       Save TSYS line for later user
               LTSYS = LINE
C                                       Replace 'timeoff = *' with
C                                       'timeoff = 0' if found
               LMATCH = 'timeoff = *'
               CALL MATCH (LMATCH, LTSYS, IFND, N1, N2)
               IF ((IFND.GT.0).AND.(N2.GT.0)) THEN
                  J = N2 + 10
                  LTSYS(J:J) = '0'
                  END IF
C                                       Replace '/' with blank
C                                       in TSYS line
               LMATCH = '/'
               CALL MATCH (LMATCH, LTSYS, IFND, N1, N2)
               IF ((IFND.GT.0).AND.(N2.GT.0)) THEN
                  LTSYS(N2:N2) = ' '
                  END IF
C                                       Don't print TSYS line now
               WSKIP = WTRUE
C                                       Last line type read
               LAST = 'TSYS'
C----------------------------------------------------------------------
C                                       b: VLBA FREQ comment line
            ELSE IF (((LINE(14:16).EQ.'RCP').OR.(LINE(14:16).EQ.'LCP'))
     *         .AND.(LINE(1:1).EQ.'!')) THEN
C                                       Extract column number
               READ (LINE(2:4),9200) ICOL
C                                       If col=1 the reset INDEX record
               IF (ICOL.EQ.1) THEN
                  DO 200 J = 1, MAXCOL
                     LPOL(J) = ' '
200                  CONTINUE
                  CALL FILL (MAXCOL, 0, IIF)
                  NFILL = 2 * MAXCOL
                  CALL DFILL (NFILL, 0.0D0, DFQIF)
C                                       Terminate preceding TSYS
C                                       sub-section ?
                  IF (.NOT.WFRST2) THEN
                     NSAVE = 1
                     LSAVE(1) = '/'
                     WSAVE = WTRUE
                     END IF
                  WFRST2 = WFALSE
                  END IF
               NCOL = MAX (ICOL, NCOL)
C                                       Extract polarization
               IF (LINE(14:16).EQ.'RCP') LPOL(ICOL) = 'R'
               IF (LINE(14:16).EQ.'LCP') LPOL(ICOL) = 'L'
C                                       Center frequency (MHz)
               READ (LINE(37:46),9250) DCENT
               DCENT = ABS (DCENT)
C                                       Bandwidth (MHz)
               READ (LINE(32:35),9260) DBW
C                                       Edge frequencies (MHz)
               DFQIF(ICOL,1) = DCENT - DBW / 2.0D0
               DFQIF(ICOL,2) = DCENT + DBW / 2.0D0
C                                       Last line type read
               LAST = 'COMNT_FQ'
C----------------------------------------------------------------------
C                                       d: TSYS data values
            ELSE IF (LINE(7:7).EQ.':') THEN
C                                       Start of TSYS sub-section ?
               IF (LAST.NE.'TSYS_VAL') THEN
C                                       Restore TSYS header
                  NSAVE = 1
                  LSAVE(1) = LTSYS
C                                       Enable printing of lines
C                                       of the stack
                  WSAVE = WTRUE
C                                       Check if last line was a
C                                       frequency comment line
                  IF (LAST.EQ.'COMNT_FQ') THEN
C                                       Match frequencies against
C                                       IF numbers in AIPS uv-file
                     CALL FQIF (DFQIF, IIF, MAXCOL, NCOL, FQTOL, IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1300) LINE(1:12)
                        CALL MSGWRT (6)
                        WRITE (MSGTXT,1320)
                        CALL MSGWRT (6)
                     ELSE
C                                       Add INDEX string
                        NSAVE = NSAVE + 1
                        LSAVE(NSAVE) = 'INDEX ='
                        DO 310 J = 1, NCOL
C                                       Decide if INDEX entry
C                                       should be blanked
                           WBAD = (IIF(J).LE.0)
                           IF (NPOLUV.EQ.1) THEN
                              IF ((IPOLUV.EQ.-1).AND.(LPOL(J).EQ.'L'))
     *                           WBAD = WTRUE
                              IF ((IPOLUV.EQ.-2).AND.(LPOL(J).EQ.'R'))
     *                           WBAD = WTRUE
                              END IF
C
                           IF (WBAD) THEN
                              WRITE (LTMP7,9305)
                           ELSE
                              WRITE (LTMP7,9310) LPOL(J), IIF(J)
                              END IF
C
                           N1 = JTRIM (LSAVE(NSAVE))
                           N2 = JTRIM (LTMP7) - 1
                           IF (J.EQ.NCOL) THEN
                              LSAVE(NSAVE) = LSAVE(NSAVE)(1:N1) //
     *                           LTMP7(1:N2)
                           ELSE
                              LSAVE(NSAVE) = LSAVE(NSAVE)(1:N1) //
     *                           LTMP7
                              END IF
310                        CONTINUE
                       END IF
                     END IF
C                                       Add backslash '/'
                  N1 = JTRIM (LSAVE(NSAVE))
                  LSAVE(NSAVE) = LSAVE(NSAVE)(1:N1) // '/'
                  END IF
C                                       Last line type read
               LAST = 'TSYS_VAL'
C----------------------------------------------------------------------
C                                       e: Backslash '/'
            ELSE IF (LINE(1:1).EQ.'/') THEN
C                                       Pass through directly
            ELSE IF (LINE(1:1).NE.'!') THEN
C                                       Unidentified: Prepend '!'
               LOUT = '!' // LOUT(1:131)
               END IF
C                                       Endcase (TSYS line type)
C======================================================================
C                                       2: Flag data
         ELSE IF (LSTATN.EQ.'FLAG') THEN
C                                       Case line type of:
C----------------------------------------------------------------------
C                                       a: opcode='FLAG'
C                                       b: dtimrang = n
C                                       c: ant_name='xx
C                                       d: timeoff=0
C                                       e: /
            IF ((LINE(1:6).EQ.'opcode').OR.
     *         (LINE(1:8).EQ.'dtimrang').OR.
     *         (LINE(1:8).EQ.'ant_name').OR.
     *         (LINE(1:9).EQ.'timeoff=0').OR.
     *         (LINE(1:1).EQ.'/')) THEN
C                                       Pass through directly
               CONTINUE
C----------------------------------------------------------------------
C                                       f: timeoff = *
            ELSE IF (LINE(1:11).EQ.'timeoff = *') THEN
C                                       Replace * by 0
               LMATCH = 'timeoff = *'
               CALL MATCH (LMATCH, LINE, IFND, N1, N2)
               IF ((IFND.GT.0).AND.(N2.GT.0)) THEN
                  J = N2 + 10
                  LOUT(J:J) = '0'
                  END IF
C----------------------------------------------------------------------
C                                       g: antennas = vlba_xx
            ELSE IF (LINE(1:16).EQ.'antennas = vlba_') THEN
C                                       Match uppercase antenna name
C                                       in AN table
               LANT = LINE(17:18)
               CALL CHLORU ('UP', 2, LANT)
               JANT = 0
               DO 400 J = 1, NOSTA
                  IF (STNNAM(J).EQ.LANT) JANT = TELNO(J)
400               CONTINUE
C                                       Error if no match
               IF (JANT.LE.0) THEN
                  IRET = 4
                  WRITE (MSGTXT,1400) LANT
                  GO TO 990
                  END IF
C                                       Extract day numbers
               READ (LINE(32:34),9400) IDAY1
               READ (LINE(47:49),9400) IDAY2
C                                       Refer to cat. hdr. ref. day
               IDAY1 = IDAY1 - IREFDY
               IDAY2 = IDAY2 - IREFDY
C                                       Reconstruct output line
               N1 = JTRIM (LINE)
               WRITE (LOUT,9420) JANT, IDAY1, LINE(35:46), IDAY2
               N2 = JTRIM (LOUT)
               LOUT = LOUT(1:N2) // LINE(50:N1)
C----------------------------------------------------------------------
C                                       h: Unidentified; prepend !
            ELSE IF (LINE(1:1).NE.'!') THEN
               LOUT = '!' // LOUT(1:131)
               END IF
C======================================================================
C                                       3: WEATHER (WX) section
         ELSE IF (LSTATN.EQ.'WX') THEN
C                                       Case line type of:
C----------------------------------------------------------------------
C                                       a: Header line
            IF (LINE(1:27).EQ.'* ----- Weather information') THEN
C                                       Extract station name
               LANT = LINE(33:34)
C                                       Replace * in col. 1
               LOUT(1:1) = '!'
C                                       Mark start of new WX section
               WFRST2 = WTRUE
C                                       Terminate prev. WX section
               IF (.NOT.WFRST1) THEN
                  NSAVE = 1
                  LSAVE(1) = '/'
                  WSAVE = WTRUE
                  END IF
               WFRST1 = WFALSE
C----------------------------------------------------------------------
C                                       b: WX data values
            ELSE IF ((LINE(4:4).EQ.'-').AND.(LINE(7:7).EQ.':')) THEN
C                                       Comment out if any * present
               LMATCH = '*'
               CALL MATCH (LMATCH, LINE, IFND, N1, N2)
               IF ((IFND.GT.0).AND.(N2.GT.0)) THEN
                  LOUT = '!' // LOUT(1:131)
                  END IF
C                                       Insert WEATHER header if
C                                       first line of WX data
               IF (WFRST2) THEN
                  NSAVE = 1
                  LSAVE(NSAVE) = 'WEATHER ' // LANT // ' /'
                  WSAVE = WTRUE
                  WFRST2 = WFALSE
                  END IF
C----------------------------------------------------------------------
C                                       c: Unidentified; Prepend !
            ELSE IF (LINE(1:1).NE.'!') THEN
               LOUT = '!' // LOUT(1:131)
               END IF
C======================================================================
C                                       4: Gain section
         ELSE IF (LSTATN.EQ.'GAIN') THEN
C                                       Case line type of:
C----------------------------------------------------------------------
C                                       a: BAND
            IF (LINE(1:5).EQ.' BAND') THEN
C                                       Search for matching freq. code
               JCODE = 0
               DO 600 J = 1, MXCODE
                  CALL MATCH (LFCODE(J), LINE, IFND, N1, N2)
C                                       Freq. code identified
                  IF ((IFND.GT.0).AND.(N2.GT.0)) JCODE = J
600               CONTINUE
C                                       Is this gain info. required ?
               WANTD = WFALSE
               IF (JCODE.GT.0) THEN
                  DO 640 J = 1, NFQUV
                     DO 630 K = 1, NIFTB
                        DO 620 M = 1, 2
C                                       Convert to GHz
                           DFGHZ = DFRQTB(J,K,M) / 1000.0
                           IF ((DFGHZ.GE.DFRQLO(JCODE)).AND.
     *                         (DFGHZ.LE.DFRQHI(JCODE))) WANTD = WTRUE
620                        CONTINUE
630                     CONTINUE
640                  CONTINUE
                  END IF
C                                       Check time range
               IF (WANTD) THEN
C                                       Extract start time
                  READ (LINE(34:37),9600) IYR1
                  READ (LINE(39:40),9610) IMNTH1
                  READ (LINE(42:43),9610) IDAY1
                  READ (LINE(45:46),9610) IHR1
C                                       Fractional year
                  CALL DAYYR (IYR1, IMNTH1, IDAY1, IDAYN1, ILOYR1)
                  DYEAR1 = DBLE (IDAYN1) / DBLE (ILOYR1) + IYR1 +
     *               DBLE (IHR1) / 24.0
C                                       Extract end time
                  READ (LINE(50:53),9600) IYR2
                  READ (LINE(55:56),9610) IMNTH2
                  READ (LINE(58:59),9610) IDAY2
                  READ (LINE(61:62),9610) IHR2
C                                       Fractional year
                  CALL DAYYR (IYR2, IMNTH2, IDAY2, IDAYN2, ILOYR2)
                  DYEAR2 = DBLE (IDAYN2) / DBLE (ILOYR2) + IYR2 +
     *               DBLE (IHR2) / 24.0
C                                       Is this time range selected ?
                  WANTD = (DYEAR1.LE.DREFYR(1)).AND.
     *               (DREFYR(2).LE.DYEAR2)
                  END IF
C                                       Skip if not wanted
               WSKIP = (.NOT.WANTD)
               LOUT = '!' // LOUT(1:131)
C----------------------------------------------------------------------
C                                       b: FREQ
C                                       c: TS
            ELSE IF ((LINE(1:5).EQ.' FREQ').OR.
     *         (LINE(1:3).EQ.' TS')) THEN
C                                       Skip if not wanted
               WSKIP = (.NOT.WANTD)
               LOUT = '!' // LOUT(1:131)
C----------------------------------------------------------------------
C                                       d: GAIN record
            ELSE
               LMATCH = 'GAIN'
               CALL MATCH (LMATCH, LINE, IFND, N1, N2)
               IF ((IFND.GT.0).AND.(N2.GT.0)) THEN
C                                       Skip if not wanted
                  WSKIP = (.NOT.WANTD)
C                                       notify user
                  IF (WANTD) THEN
                     IF (IPRTLV.GT.0) THEN
                        WRITE (MSGTXT, 19998) LINE(1:8), LFCODE(JCODE),
     *                     DFRQLO(JCODE), DFRQHI(JCODE)
                        CALL MSGWRT (6)
                        END IF
C                                       attempt to add FREQ = to end of
C                                       line
                     LMATCH = '/'
                     CALL MATCH (LMATCH, LOUT, IFND, N1, N2)
                     N1 = N2
                     N2 = N1 + 22
                     IF ((N2.LE.132).AND.(IFND.GT.0).AND.(N1.GT.0)) THEN
                        WRITE (LOUT(N1:N2), 19999) 1000.*DFRQLO(JCODE),
     *                     1000.*DFRQHI(JCODE)
                        END IF
                     END IF
C----------------------------------------------------------------------
C                                       e: Unidentified; prepend !
               ELSE IF (LINE(1:1).NE.'!') THEN
                  LOUT = '!' // LOUT(1:131)
                  END IF
               END IF
C======================================================================
C                                       5: Pulse calibration section
         ELSE IF (LSTATN.EQ.'PCAL') THEN
C                                       Copy through directly
C======================================================================
C                                       6: Scan section
         ELSE IF (LSTATN.EQ.'SCAN') THEN
C                                       Copy through directly
C======================================================================
C                                       7: MKIII section
         ELSE IF (LSTATN.EQ.'MKIII') THEN
C                                       Copy through directly
C======================================================================
C                                       Other: skip output
         ELSE
            WSKIP = WTRUE
            END IF
C                                       Endcase (State)
C                                       Write lines on stack if req.
         IF (WSAVE) THEN
            DO 850 J = 1, NSAVE
               N1 = JTRIM (LSAVE(J))
               CALL ZTXIO ('WRIT', ILUNO, IFINDO, LSAVE(J)(1:N1), IERR)
               IF (IERR.NE.0) THEN
                  IRET = 8
                  WRITE (MSGTXT,1800) IERR, LFILE
                  GO TO 990
                  END IF
850            CONTINUE
            NSAVE = 0
            ENDIF
C                                       Write current line
         IF (.NOT.WSKIP) THEN
            N1 = JTRIM (LOUT)
            CALL ZTXIO ('WRIT', ILUNO, IFINDO, LOUT(1:N1), IERR)
            IF (IERR.NE.0) THEN
               IRET = 8
               WRITE (MSGTXT,1800) IERR, LFILE
               GO TO 990
               END IF
            END IF
C                                       Loop back to read next line
         GO TO 100
C                                       Endwhile
900   IF (WERROR) THEN
         WRITE (MSGTXT,1900) IERR, LINFIL
         IRET = 9
         GO TO 990
         END IF
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit: hard close of text files
999   CALL ZTXCLS (ILUNO, IFINDO, JERR)
      CALL ZTXCLS (ILUNF, IFINDF, JERR)
      RETURN
C----------------------------------------------------------------------
1100  FORMAT ('VLCAL: ERR',I3,' OPENING ',A48)
1150  FORMAT ('VLCAL: OUTPUT FILE PREFIX TOO LONG; SHORTEN OUTFILE')
1300  FORMAT ('***** NO INDEX RECORD FOR LINE STARTING: ',A12)
1320  FORMAT ('***** NO UNIQUE FREQUENCY MATCH FOUND')
1400  FORMAT ('VLCAL: UNIDENTIFIED ANT. NAME: ',A2,' IN EDIT SECTION')
1800  FORMAT ('VLCAL: ERR',I3,' OPENING ',A48)
1900  FORMAT ('VLCAL: ERR',I3,' READING ',A48)
9200  FORMAT (I3)
9250  FORMAT (D10.2)
9260  FORMAT (F4.0)
9305  FORMAT (5H 'X',)
9310  FORMAT (1X,1H',A1,I2.2,2H',)
9400  FORMAT (I3)
9420  FORMAT ('antennas = ',I3,' timerang = ',I3,A12,I3)
9600  FORMAT (I4)
9610  FORMAT (I2)
19998 FORMAT ('Passing ',A8,A9,' GAIN curve for use between ',F7.3,
     *   ' and ',F7.3,' GHz')
19999 FORMAT ('FREQ= ',F7.0,',',F7.0,' /')
      END
      SUBROUTINE STATE (LINE, LSTATN)
C----------------------------------------------------------------------
C   Identify the start of a new section in the cal. file
C   Inputs:
C      LINE       C*132      Input text line
C   Input/Output:
C      LSTATN     C*8        State:
C                            'NULL':  no identified section
C                            'TSYS':  Tsys section
C                            'FLAG':  Flag section
C                            'WX':    Weather section
C                            'GAIN':  Gain section
C                            'PCAL':  Pulse-cal. section
C                            'SCAN':  Scan section
C                            'MKIII': MkIII section
C----------------------------------------------------------------------
      CHARACTER LINE*132, LSTATN*8
C----------------------------------------------------------------------
C                                       Tsys
      IF (LINE(1:24).EQ.'! ----- Tsys information') THEN
         LSTATN = 'TSYS'
         END IF
C                                       Flag
      IF (LINE(1:17).EQ.'! ----- Edit data') THEN
         LSTATN = 'FLAG'
         END IF
C                                       Weather
      IF (LINE(1:27).EQ.'* ----- Weather information') THEN
         LSTATN = 'WX'
         END IF
C                                       Gain
      IF (LINE(1:8).EQ.' BAND = ') THEN
         LSTATN = 'GAIN'
         END IF
C                                       Pulse-cal
      IF (LINE(1:28).EQ.'! ----- PulseCal information') THEN
         LSTATN = 'PCAL'
         END IF
C                                       Scan
      IF (LINE(1:24).EQ.'! ----- Scan information') THEN
         LSTATN = 'SCAN'
         END IF
C                                       MkIII
      IF (LINE(1:25).EQ.'* ----- MkIII information') THEN
         LSTATN = 'MKIII'
         END IF
C                                       Exit
999   RETURN
      END
      SUBROUTINE FQREAD (IRET)
C-----------------------------------------------------------------------
C   Subroutine to read parts of the FQ table into memory
C   Outputs to common:
C      DFRQTB  D(*,*,2) Table of freq. values for each IF/FQ-ID
C   Outputs:
C      IRET    I    Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION DFOFF(MAXIF), DBAND, DEDGE, DREF
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF), NIF, I, J, IVER, IERR, NDIM
      CHARACTER BNDCOD(MAXIF)*8
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialization
      NDIM = 2 * MXFQID * MAXIF
      CALL DFILL (NDIM, 0.0D0, DFRQTB)
C                                       Reference freq. at DC
*      DREF = FREQ - (CATR(KRCRP+JLOCF) - 0.5) * CATR(KRCIC+JLOCF)
      DREF = FREQ - (CATR(KRCRP+JLOCF) - 1.0) * CATR(KRCIC+JLOCF)
      NIFTB = 0
C                                       Read FQ/CH data into memory
      DO 100 I = 1, NFQUV
         IVER = 1
         CALL CHNDAT ('READ', JBUFF1, INDISK, ICNO, IVER, CATBLK,
     *      ILUNTB, NIF, DFOFF, ISBAND, FINC, BNDCOD, IFQUV(I), IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Max. no. of IFs
         NIFTB = MAX (NIF, NIFTB)
C                                       Loop over the IFs
         DO 80 J = 1, NIF
            DBAND = FINC(J) * CATBLK(KINAX+JLOCF)
            DEDGE = DFOFF(J) + DREF
            IF (ISBAND(J).EQ.-1) DEDGE = DEDGE - DBAND
C                                       Convert to MHz
            DFRQTB(I,J,1) = DEDGE / 1.0D6
            DFRQTB(I,J,2) = (DEDGE + DBAND) / 1.0D6
80          CONTINUE
100      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C-----------------------------------------------------------------------
1100  FORMAT ('FQREAD: ERR',I3,' READING FQ/CH TABLE')
      END
      SUBROUTINE NXREAD (IRET)
C-----------------------------------------------------------------------
C   Subroutine to read the NX table into memory
C   Outputs:
C      IRET    I    Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTABLE, WEXIST, WFITS
      REAL TIME, TINT
      INTEGER NXKOLS(MAXNXC), NXNUMV(MAXNXC), IERR, I, INXRNO, IDSOUR,
     *   ISUBNX, ISTART, IEND, IFQID, NROW, NVISMX, J
C-----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      NXDAT = 0
C                                       Does an NX table exist ?
      CALL ISTAB ('NX', INDISK, ICNO, 1, ILUNTB, JBUFF1, WTABLE,
     *   WEXIST, WFITS, IERR)
      IF ((IERR.NE.0).OR.(.NOT.(WEXIST.AND.WTABLE))) THEN
         IRET = 1
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Open the NX table
      CALL NDXINI ('READ', JBUFF1, INDISK, ICNO, 1, CATBLK, ILUNTB,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Read table into memory
      NROW = JBUFF1(5)
      NVISMX = 0
      DO 100 I = 1, NROW
         CALL TABNDX ('READ', JBUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      TINT, IDSOUR, ISUBNX, ISTART, IEND, IFQID, IERR)
C                                       Record de-selected ?
         IF (IERR.EQ.-1) GO TO 100
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Update maximum vis. no.
         NVISMX = MAX (NVISMX, IEND)
C                                       Correct subarray ?
         IF (ISUBNX.NE.ISUBA) GO TO 100
C                                       NX buffer too small ?
         NXDAT = NXDAT + 1
         IF (NXDAT.GT.MAXNX) THEN
            IRET = 4
            WRITE (MSGTXT,1120)
            GO TO 990
            END IF
C
         TIMENX(1,NXDAT) = TIME - TINT / 2.0
         TIMENX(2,NXDAT) = TIME + TINT / 2.0
         INXSOU(NXDAT) = IDSOUR
         INXFQ(NXDAT) = IFQID
100      CONTINUE
C                                       Close NX table
      CALL TABIO ('CLOS', 0, INXRNO, JBUFF2, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1140) IERR
         GO TO 990
         END IF
C                                       Determine whether NX table is
C                                       current by matching Nvis in
C                                       catalog hdr. with NVISMX from
C                                       NX table.
      IF (NVISMX.NE.CATBLK(KIGCN)) THEN
         IRET = 5
         WRITE (MSGTXT,1160)
         GO TO 990
         END IF
C                                       Any valid scans found ?
      IF (NXDAT.EQ.0) THEN
         IRET = 6
         WRITE (MSGTXT,1180)
         GO TO 990
         END IF
C                                       Compile table of all unique
C                                       FQ_IDs in the data. Use
C                                       information read from the NX
C                                       table.
      NFQUV = 0
      DO 200 I = 1, NXDAT
         DO 150 J = 1, NFQUV
            IF (IFQUV(J).EQ.INXFQ(I)) GO TO 200
150         CONTINUE
C                                       New FQ-ID found
         NFQUV = NFQUV + 1
         IF (NFQUV.GT.MXFQID) THEN
            WRITE (MSGTXT,1150)
            IRET = 11
            GO TO 990
            END IF
         IFQUV(NFQUV) = INXFQ(I)
200      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C-----------------------------------------------------------------------
1020  FORMAT ('NXREAD: NO VALID NX TABLE FOUND - RUN INDXR')
1040  FORMAT ('NXREAD: ERR',I3,' OPENING NX TABLE')
1100  FORMAT ('NXREAD: ERR',I3,' READING NX TABLE')
1120  FORMAT ('NXREAD: INCREASE PARAMETER MAXNX')
1140  FORMAT ('NXREAD: ERR',I3,' CLOSING NX TABLE')
1160  FORMAT ('NXREAD: NX TABLE OLD - RUN INDXR')
1180  FORMAT ('NXREAD: NO SCANS FOR SUBARRAY',I4)
1150  FORMAT ('NXREAD: PARAMETER MXFQID NEEDS TO BE INCREASED')
      END
      SUBROUTINE VLOGHI (IRET)
C----------------------------------------------------------------------
C   Update history file for task VLOG
C   Output:
C      IRET     I       Return code (0=>ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      LOGICAL WUPDAT
      CHARACTER LHIREC*72, LTIME*20
      INTEGER IERR, IDATE(3), ITIME(3)
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C
      CALL HIINIT (3)
C                                       Open history table
      CALL HIOPEN (ILUNTB, INDISK, ICNO, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Task name and time
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, LTIME(13:20), LTIME(1:12))
      WRITE (LHIREC,1010) TSKNAM, RLSNAM, LTIME(1:12), LTIME(13:20)
      CALL HIADD (ILUNTB, LHIREC, JBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       UV-file name
      WRITE (LHIREC,1020) TSKNAM, LNAME, LCLASS, INDISK, INSEQ
      CALL HIADD (ILUNTB, LHIREC, JBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Subarray
      WRITE (LHIREC,1030) TSKNAM, ISUBA
      CALL HIADD (ILUNTB, LHIREC, JBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Input file
      WRITE (LHIREC,1040) TSKNAM, LINFIL
      CALL HIADD (ILUNTB, LHIREC, JBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Output file
      WRITE (LHIREC,1050) TSKNAM, LOUTFL
      CALL HIADD (ILUNTB, LHIREC, JBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close HI file
      WUPDAT = .TRUE.
      CALL HICLOS (ILUNTB, WUPDAT, JBUFF1, IERR)
      IF (IERR.EQ.0) GO TO 995
C                                       Error
980   WRITE (MSGTXT,1980) IERR
C
990   CALL MSGWRT (8)
C                                       reset status
 995  CALL CATIO ('UPDT', INDISK, ICNO, CATBLK, 'CLWR', JBUFF1, IERR)
      IF (IERR.EQ.0) NCFILE = 0
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('VLOGHI: ERROR',I3,' OPENING HI TABLE')
1010  FORMAT (A6,' RELEASE: ',A8,' START TIME: ',A12,2X,A8)
1020  FORMAT (A6,' INNAME= ',A12,'.',A6,'.',I3,'.',I4)
1030  FORMAT (A6,' SUBARRAY= ',I3)
1040  FORMAT (A6,' INFILE= ',A48)
1050  FORMAT (A6,' OUTFILE= ',A48)
1980  FORMAT ('VLOGHI: ERROR',I3,' PROCESSING HISTORY FILE')
      END
      SUBROUTINE FQIF (DFRQ, IIF, MAXCOL, N, TOL, IRET)
C----------------------------------------------------------------------
C   Match TSYS column numbers to uv-data IF numbers
C   Inputs:
C      DFRQ       D(MAXCOL,2)     Lower,upper freq. in ext. cal. file
C      MAXCOL     I               First dimension of DFRQ
C      N          I               No. of elements in DFRQ, IIF
C      TOL        R               Tolerance used in freq. match (Hz)
C   Output:
C      IIF        I(N)            Column-IF assignments
C      IRET       I               Return code (0=>ok; else no match)
C----------------------------------------------------------------------
      INTEGER MAXCOL, N, IIF(N), IRET
      DOUBLE PRECISION DFRQ(MAXCOL,2)
      REAL TOL
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'VLOG.INC'
      LOGICAL WMATCH(MXFQID), WMCOL
      INTEGER IIFWRK(MAXIF,MXFQID), IFQID, KCOL, MIF, NMATCH, JMATCH
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NMATCH = 0
      CALL FILL (N, 0, IIF)
C                                       Loop over FQ_ID
      DO 500 IFQID = 1, NFQUV
         CALL FILL (N, 0, IIFWRK(1,IFQID))
         WMATCH(IFQID) = .TRUE.
C                                       Loop over column
         DO 400 KCOL = 1, N
            WMCOL = .FALSE.
            MIF = 1
C                                       While (M<Nif) and
C                                          (NOT match) do:
200         IF ((MIF.GT.NIFTB).OR.WMCOL) GO TO 300
               WMCOL = ((ABS (DFRQ(KCOL,1)-DFRQTB(IFQID,MIF,1)).LT.TOL)
     *            .AND.(ABS (DFRQ(KCOL,2)-DFRQTB(IFQID,MIF,2)).LT.TOL))
               MIF = MIF + 1
               GO TO 200
C                                       Endwhile
300         IF (WMCOL) THEN
               MIF = MIF - 1
               IIFWRK(KCOL,IFQID) = MIF
               END IF
400         CONTINUE
C                                       Check for match of all IFs
C                                       in uv-file
            DO 450 MIF = 1, NIFTB
               DO 425 KCOL = 1, N
                  IF (IIFWRK(KCOL,IFQID).EQ.MIF) GO TO 450
425               CONTINUE
               WMATCH(IFQID) = .FALSE.
450            CONTINUE
C                                       Update count
            IF (WMATCH(IFQID)) NMATCH = NMATCH + 1
500      CONTINUE
C                                       No unique match found ?
      IF ((NMATCH.EQ.0).OR.(NMATCH.GT.1)) THEN
         IRET = 1
         GO TO 999
      ELSE
C                                       Return column-IF assignments
         JMATCH = 0
         DO 550 IFQID = 1, NFQUV
            IF (WMATCH(IFQID)) JMATCH = IFQID
550         CONTINUE
         CALL COPY (N, IIFWRK(1,JMATCH), IIF)
         END IF
      GO TO 999
C                                       Exit
999   RETURN
      END
      SUBROUTINE DAYYR (IYR, IMONTH, IDAY, IDAYNO, ILOYR)
C-----------------------------------------------------------------------
C   Subroutine to compute day number (Jan 1 = 1) from Gregorian date
C   Inputs:
C      IYR     I   Year number (eg. 1994)
C      IMONTH  I   Month number (Jan=1, Feb=2,.., Dec=12)
C      IDAY    I   Day of month
C   Outputs:
C      IDAYNO  I   Day number (Jan. 1 = 1)
C      ILOYR   I   No. of days in year (365 or 366)
C----------------------------------------------------------------------
      INTEGER IYR, IMONTH, IDAY, IDAYNO, ILOYR
C
      LOGICAL WLEAP
      INTEGER IDSUM(12)
      DATA IDSUM /0,31,59,90,120,151,181,212,243,273,304,334/
C----------------------------------------------------------------------
      IDAYNO = IDSUM(IMONTH) + IDAY
      ILOYR = 365
C                                       Leap year ?
      WLEAP = (MOD(IYR,4).EQ.0)
C                                       Centurial leap years ?
      IF (WLEAP.AND.(MOD(IYR,100).EQ.0)) WLEAP = (MOD(IYR,400).EQ.0)
C
      IF (WLEAP.AND.(IMONTH.GT.2)) IDAYNO = IDAYNO + 1
      IF (WLEAP) ILOYR = 366
      RETURN
      END
      SUBROUTINE LINCHK (LINE)
C-----------------------------------------------------------------------
C   Checks LINE for non-ascii characters - substituting ? and for the
C   string 'NaN' substituting 999.
C   In/Out:
C      LINE   C*(*)   Character string to test/fix
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
C
      INTEGER   I, L, WASERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      L = LEN (LINE)
      WASERR = 0
C                                       search for non-ascii, NaN
      DO 10 I = 1,L
         IF (LINE(I:I).LT.' ') THEN
            WASERR = (WASERR / 2) * 2 + 1
            LINE(I:I) = '?'
            END IF
         IF ((I.LE.L-4) .AND. (LINE(I:I+4).EQ.' NaN ')) THEN
            WASERR = MOD (WASERR, 2) + 2
            LINE(I+1:I+3) = '999'
            END IF
 10      CONTINUE
C                                       beginning and end of line
      IF ((LINE(:4).EQ.'NaN ')) THEN
            WASERR = MOD (WASERR, 2) + 2
            LINE(:4) = '999 '
            END IF
      IF ((LINE(L-3:L).EQ.' NaN')) THEN
            WASERR = MOD (WASERR, 2) + 2
            LINE(L-2:) = '999'
            END IF
C                                       error message
      IF (WASERR.GT.0) THEN
         IF (WASERR.EQ.1) THEN
            MSGTXT = '? SUBSTITUTED FOR BAD CHARACTER(S) IN'
         ELSE IF (WASERR.EQ.2) THEN
            MSGTXT = '999 SUBSTITUTED FOR NaN IN'
         ELSE
            MSGTXT = '999 SUBSTITUTED FOR NaN and 999 FOR ' //
     *         'BAD CHARACTER(S) IN'
            END IF
         CALL MSGWRT (6)
         L = MIN (L, 80)
         MSGTXT = LINE(1:L)
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
