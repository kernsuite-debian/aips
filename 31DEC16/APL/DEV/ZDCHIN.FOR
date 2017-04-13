       SUBROUTINE ZDCHIN (DODISK)
C-----------------------------------------------------------------------
C! initialize message, device and Z-routine characteristics commons
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2004, 2006-2009, 2011
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
C   Initialize the device characteristics common and the FCB's (file
C   control blocks) in FTAB(*) for the maximum number of different file
C   types that can be open at the same time.  Initialize also other
C   machine-dependent commons and the message common.  Note that the
C   task name is not set here.
C   Inputs:
C      DODISK   L        Get SETPAR-controlled parameters from disk
C   Inputs from common: DMSG.INC
C      TSKNAM   C*6      Task name if known - else ' ' (used in ABORT
C                        handler mostly to separate standalones and
C                        tasks)
C   Output in commons: DDCH.INC DMSG.INC
C      all      ...      All values set to init except TSKNAM
C
C   ZDCHIN starts with hard-coded values.  Then, if DODISK is true,
C   resets those contained in the system parameter file.  The utility
C   program SETPAR is used to alter the system parameter file values.
C
C   Critical system constants (all "words" are local integers, all
C   "bytes" are AIPS-bytes, i.e., 1/2 a local integer and on 64 bit
C   architectures, double precision contructs should be preprocessed
C   into their single precision counterparts):
C
C   Generic version - calls ZDCHI2, ZDCHIC (init C codes).
C   ZABORT call (initialize abort handler) also - uses TSKNAM
C   Modified to incorporate ATNF modifications 91.12.04
C   Modified to handle larger NETSP files 93.05.27, 11.05.26
C   Modified to include GNU Copyleft 95.06.15
C-----------------------------------------------------------------------
      LOGICAL   DODISK
C
      INTEGER   ERRTER, SPLUN, SPIND, IERR, JERR, BLKVAL, NFTAB, I,
     *   IOBLK(256), IENT, IVOL, J, NDX, NENT, NETLUN, NGNU, IDATE(3),
     *   JTRIM, ADDRSZ
      PARAMETER (NGNU = 23)
      CHARACTER PNAME*48, DA0X*4, NETSP(1024)*192, TEXT*192, LINE*72,
     *   GPLBRK(2)*35, GPLDAT*21, GPLTXT(2,NGNU)*35, GPLTX1(2,10)*35,
     *   GPLTX2(2,10)*35, GPLTX3(2,NGNU-20)*35, SITNAM*40
      REAL      ROBLK(256)
      LOGICAL   MAP, EXCL, WAIT, NEOF
      DOUBLE PRECISION DP
      INCLUDE 'INCS:DBBF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCRE.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      EQUIVALENCE (EXCL, WAIT),       (IOBLK, ROBLK, BBUFR1)
      EQUIVALENCE (GPLTXT(1,1), GPLTX1(1,1))
      EQUIVALENCE (GPLTXT(1,11), GPLTX2(1,1))
      EQUIVALENCE (GPLTXT(1,21), GPLTX3(1,1))
      DATA ERRTER, SPLUN, BLKVAL /6, 27, -32768/
      DATA MAP, EXCL /.FALSE., .TRUE./
      DATA NETLUN /11/
      DATA GPLDAT /'  Copyright (C) 1995-'/
      DATA GPLBRK /'-----------------------------------',
     *             '-----------------------------------'/
      DATA GPLTX1 /'  Associated Universities, Inc. Was',
     *             'hington DC, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is free software; yo',
     *             'u can redistribute it and/or       ',
     *             '  modify it under the terms of the ',
     *             'GNU General Public License as      ',
     *             '  published by the Free Software Fo',
     *             'undation; either version 2 of      ',
     *             '  the License, or (at your option) ',
     *             'any later version.                 ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is distributed in th',
     *             'e hope that it will be useful,     ',
     *             '  but WITHOUT ANY WARRANTY; without',
     *             ' even the implied warranty of      ',
     *             '  MERCHANTABILITY or FITNESS FOR A ',
     *             'PARTICULAR PURPOSE.  See the       '/
      DATA GPLTX2 /'  GNU General Public License for mo',
     *             're details.                        ',
     *             '                                   ',
     *             '                                   ',
     *             '  You should have received a copy o',
     *             'f the GNU General Public           ',
     *             '  License along with this program; ',
     *             'if not, write to the Free          ',
     *             '  Software Foundation, Inc., 675 Ma',
     *             'ssachusetts Ave, Cambridge,        ',
     *             '  MA 02139, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  Correspondence concerning AIPS sh',
     *             'ould be addressed as follows:      ',
     *             '         Internet email: aipsmail@n',
     *             'rao.edu.                           ',
     *             '         Postal address: AIPS Proje',
     *             'ct Office                          '/
      DATA GPLTX3 /'                         National R',
     *             'adio Astronomy Observatory         ',
     *             '                         520 Edgemo',
     *             'nt Road                            ',
     *             '                         Charlottes',
     *             'ville, VA 22903-2475 USA           '/
C-----------------------------------------------------------------------
      I = JTRIM (TSKNAM)
      IF (I.LE.0) TSKNAM = 'ZDCHIN'
C                                       Establish abort handling
      CALL ZABORT (TSKNAM, 0)
C                                       Init MS file size
      MSGCNT = -1
      MSGREC = 0
C                                       init TV not open
      TVBROK = -1
C                                       Assume basic POPS #, manager
      NPOPS = 1
      NLUSER = 1
C                                       Account location unknown
      NACOUN = 0
C                                       No message suppress.
      MSGSUP = 0
C                                       No message kill.
      MSGKIL = 0
C                                       Not batch.
      ISBTCH = 0
C                                       No debug messages
      DBGAIP = 0
C                                       Default version reset by ZMYVER
      VERNAM = 'NEW:'
C                                       Get hard-coded AIPS release
C                                       ID string (i.e., 15MMMYY).
      CALL GETRLS
C                                       # bits per "word" (calculated in
C                                       ZDCHIC).
      NBITWD = 32
C                                       # bits per character.
      NBITCH = 7
C                                       # characters per input line.
      NCHLIN = 80
C                                       # "words" per double precision
C                                       floating-point (substitute
C                                       single precision on 64-bit
C                                       architectures; calculated in
C                                       ZDCHIC).
      NWDPDP = 2
C                                       # "bytes" per disk sector.
      NBPS = 512
C                                       # sectors per disk granule.
      NSPG = 8
C                                       Define FTAB (i.e., file table)
C                                       entries for the different file
C                                       types:
C
C                                       # "bytes" per non-FCB file
C                                       entry (i.e., no file control
C                                       block) in FTAB(*) where:
      NBTB1 = 28
C                                       bytes   contents
C                                       01-02   logical unit number
C                                       03-04   system error code
C                                       05-06   system file descriptor
C                                       07-28   spare (name?)
C                                       file descriptor if needed
      NFCBFD = 1
C                                       error code return
      NFCBER = 2
C
C                                       # "bytes" per non-map (single
C                                       buffered, synchronous I/O) file
C                                       control block in FTAB(*) where:
      NBTB2 = 44
C                                       bytes   contents
C                                       01-02   logical unit number
C                                       03-04   file descriptor
      FCBFD = 0
C                                       05-06   8-bit byte I/O request
      FCBREQ = 1
C                                       07-08   system error code
      FCBERR = 2
C                                       09-10   8-bit bytes transferred
      FCBXFR = 3
C                                       11-14   8-bit byte file size
C                                       15-18   8-bit byte I/O position
      FCBSIZ = 4
C                                       19-44   spare
C                                       VMS uses FCBSP1 for event flag
C                                       FCBSP2/3 for IOstatus block
C                                       Virtual tapes use 1 & 4
      FCBSP1 = 8
      FCBSP2 = 9
      FCBSP3 = 10
      FCBSP4 = 11
C                                       Offset from the beginning of an
C                                       FTAB entry for a non-map file to
C                                       the start of its file control
C                                       block.
      NMOFF = 1
C                                       # elements in FTAB for non-map
C                                       file entries.
      NMFCB = NBTB2 / 2 - NMOFF
C                                       # "bytes" per map (double
C                                       buffered, asynchronous I/O) file
C                                       control block in FTAB(*) where:
      NBTB3 = 116
C                                       bytes   contents (both buffers)
C                                       01-02   logical unit number
C                                       03-32   (used by applications)
C
C                                       The rest is the same as for
C                                       non-map files, only two file
C                                       control blocks (i.e., one for
C                                       each buffer):
C
C                                       bytes   contents (1st buffer)
C                                       33-34   file descriptor
C                                       35-36   8-bit byte I/O request
C                                       37-38   system error code
C                                       39-40   8-bit bytes transferred
C                                       41-44   8-bit byte file size
C                                       45-48   8-bit byte I/O position
C                                       49-74   spare
C
C                                       bytes   contents (2nd buffer)
C                                       75-76   file descriptor
C                                       77-78   8-bit byte I/O request
C                                       79-80   system error code
C                                       81-82   8-bit bytes transferred
C                                       83-86   8-bit byte file size
C                                       87-90   8-bit byte I/O position
C                                       91-116  spare
C
C                                       Offset from the beginning of an
C                                       FTAB entry for a map file to the
C                                       start of its file control block.
      MOFF = 16
C                                       # elements in FTAB for map file
C                                       entries (per buffer).
      MFCB = (NBTB3 / 2 - MOFF) / 2
C                                       Set /DCHCOM/ variables for the
C                                       maximum # of the different file
C                                       types that can be open at once
C                                       and initialize the required
C                                       range in FTAB(*).
      NTAB1 = 20
      NTAB2 = 80
      NTAB3 = 20
      NFTAB = (NBTB1*NTAB1 + NBTB2*NTAB2 + NBTB3*NTAB3) / 2
      CALL FILL (NFTAB, 0, FTAB)
C                                       Blank pixel indicator value.
      BLANKV = BLKVAL
C                                       Byte flip indicator code:
C                                          1 => bytes flipped
C                                          2 => words flipped (I*4 only)
C                                          3 => both
C                                          (3 for VAXes, 0 almost
C                                          everywhere else).
      BYTFLP = 0
C                                       Floating-point format code
C                                          0 => OTHER
C                                          1 => IEEE
C                                          2 => VAX F
C                                          3 => VAX G
C                                          4 => IBM (not supported yet)
C                                       Single precision
      SPFRMT = 1
C                                       Double precision
      DPFRMT = 1
C                                       # iterations considered a short
C                                       loop (if less than this, branch
C                                       to scalar code).
      NSHORT = 3
C                                       1 => TTY i/o uses carriage cntrl
      TTYCAR = 1
C                                       Initialize values for some
C                                       parameters.  If DODISK, these
C                                       will be reset with values read
C                                       in from the system parameter
C                                       file (the utility program SETPAR
C                                       is used to change values in the
C                                       system parameter file):
C
C                                       # large disks.
      NVOL = 1
C                                       # tape drives.
      NTAPED = 1
C                                       # lines per CRT page:
C                                       use # actually in window
      CRTMAX = -1
C                                       # lines per printer page.
      PRTMAX = 61
C                                       VERSATEC graphics parameters:
C                                       X, Y dots per page.
      MAXXPR(1) = 2112
      MAXXPR(2) = 1600
C                                       X, Y dots per character.
      CSIZPR(1) = 20
      CSIZPR(2) = 25
C                                       # AIPS interactive streams.
      NINTRN = 2
C                                       # AIPS batch queues.
      NBATQS = 2
C                                       # of TV devices available
C                                       (later becomes assigned TV #).
      NTVDEV = 1
C                                       # graphics devices (later
C                                       becomes assigned TK #).
      NTKDEV = 16
C                                       X dots (and Y dots until we
C                                       change format of CDCH) per
C                                       mm on printer plotter.
      XPRDMM = 7.83
C                                       X dots (and Y dots) per mm on
C                                       Tektronix graphics device.
      XTKDMM = 5.0
C                                       # POPS allowed TV, TK access.
      NTVACC = 35
      NTKACC = 35
C                                       # entries per user-private
C                                       catalog (<= 0 implies only one
C                                       big public catalog)
      UCTSIZ = 100
C                                       AIPS site system name.
      CALL ZTRLOG (4, 'SITE', 40, SITNAM, J, I)
      IF (I.NE.0) SITNAM = 'UN-REGISTERED!!!!'
      SYSNAM = SITNAM
C                                       Maximum user number (46655 max)
      USELIM = 46655
C                                       TIMDEST data limit per disk.
      CALL RFILL (35, 14.0, TIMEDA)
C                                       TIMDEST SAVE/GET files.
      TIMESG = 28.0
C                                       TIMDEST messages in message
C                                       files.
      TIMEMS = 3.0
C                                       TIMDEST scratch files.
      TIMESC = 3.0
C                                       TIMDEST empty catalog files.
      TIMECA = 0.25
C                                       Batch AP weekend no starts.
      TIMEBA(1) = 15.0
      TIMEBA(2) = 16.5
C                                       Batch AP weekday no starts.
      TIMEBA(3) = 13.0
      TIMEBA(4) = 17.0
C                                       AP roll interval (minutes).
      TIMEAP(1) = 5.0
C                                       AP delay formula (minutes) =
C                                       AP(2) + N + AP(3) * (N*N-1)
C                                       where N = NPOPS-1.
      TIMEAP(2) = 5.0
      TIMEAP(3) = 1.0
C                                       Zero spares.
      CALL RFILL (14, 0.0, RFILIT)
C                                       # characters per line-printer
C                                       line (use 132 or 72).
      NCHPRT = 132
C                                       # words in AP (in 1024s)
      CALL ZADRSZ (ADDRSZ)
C                                       pseudo-AP area
      KAPWRD = 132 * 1024
      IF (ADDRSZ.EQ.8) KAPWRD = 260 * 1024
C                                       1024s of secondary pseudo-AP
C                                       memory (192 for Alliant, Convex
C                                       and COS code supported by NRAO).
      KAP2WD = 0
C                                       magic-value blanks
      CALL CHR2H (4, 'INDE', 1, FBLANK)
      CALL CHR2H (8, 'INDEFINT', 1, DBLANK)
C                                       hollerith blank (convenience)
      CALL CHR2H (4, '    ', 1, HBLANK)
C                                       TK device size
      MAXXTK(1) = 1024
      MAXXTK(2) = 780
C                                       TK device character size
      CSIZTK(1) = 14
      CSIZTK(2) = 22
C                                       disk assignments: (n, disk#)
C                                       n = allowed user numbers
C                                           (all zero => everyone,
C                                           -1 => scratch files only)
      CALL FILL (280, 0, DASSGN)
C                                       Printer deletion time (sec)
      PRDELE = 300
C                                       Speed parameter
      AIPSMK = 20.0
C                                       Dummy space holder
      CALL FILL (NDCHD, 0, DCHDUM)
C                                       Zero device descriptor table.
C                                       DEVTAB(LUN) = 0 => disk file
C                                       with file control block in FTAB:
C
C                                       LUN = 4 (batch input)
C                                       LUN = 8 (AP roller)
C                                       LUN = 12 (message file)
C                                       LUN = 13 (task communications)
C                                       LUN = 14 (POPS "memory" file)
C                                       LUN = 15 (catalog files)
C                                       LUN = 16-25 (map files)
C                                       LUN = 26 (graphics file)
C                                       LUN = 27-30 (non-map files)
C                                       LUN = 35-50 (map or non-map)
C                                       First 50 of DEVTAB changable,
C                                       rest == 0
      CALL FILL (128, 0, DEVTAB)
C                                       DEVTAB(LUN) = odd => file with
C                                       no file control block in FTAB
C                                       (1 => Fortran device, 3 =>
C                                       other, e.g., text file):
C
C                                       LUN = 1 (line printer).
      DEVTAB(1) = 1
C                                       LUN = 3 (reserved).
      DEVTAB(3) = 3
C                                       LUN = 5 (input/output terminal).
      DEVTAB(5) = 1
C                                       LUN = 6 (output terminal).
      DEVTAB(6) = 1
C                                       LUN = 10 (POPS "run" files).
      DEVTAB(10) = 3
C                                       LUN = 11 (POPS "help" files).
      DEVTAB(11) = 3
C                                       DEVTAB(LUN) = 2 => device with
C                                       file control block in FTAB
C                                       (e.g., tape), 4 => TV device.
C
C                                       LUN = 2 (plotter).
      DEVTAB(2) = 1
C                                       LUN = 7 (Tektronix device).
      DEVTAB(7) = 2
C                                       LUN = 9 (TV device).
      DEVTAB(9) = 4
C                                       LUN = 31-39 are no longer tape
C                                       drives
C                                       clear file names
      DO 11 I = 1,NTAB1
         DEVNAM(I) = ' '
 11      CONTINUE
      DO 12 I = 1,NTAB2
         NONNAM(I) = ' '
 12      CONTINUE
      DO 13 I = 1,NTAB3
         MAPNAM(I) = ' '
 13      CONTINUE
      SYSTYP = ' '
      SYSVER = ' '
      DO 15 I = 1,35
         TPNAME(I) = ' '
 15      CONTINUE
      TPDNAM = ' '
C                                       quick file creat false
      QCREAT = -1
C                                       Call ZDCHI2 to set peculiar
C                                       variables for local system
      CALL ZDCHI2
C                                       Pass selected values to ZDCHIC
C                                       which makes them available to
C                                       routines written in C as
C                                       external variables.
      CALL ZDCHIC (NBITWD, NBITCH, NCHLIN, NWDPDP, BYTFLP, SPFRMT,
     *   DPFRMT, NBPS, NMFCB, MFCB, NFCBER, NFCBFD, FCBFD, FCBREQ,
     *   FCBERR, FCBXFR, FCBSIZ, FCBSP1, FCBSP2, FCBSP3, FCBSP4, NINTRN,
     *   SYSTYP, SYSVER)
C                                       Get the host name
      CALL ZTRLOG (4, 'HOST', 192, TEXT, NDX, JERR)
      IF ((JERR.EQ.0) .AND. (TEXT.NE.' ')) THEN
         HSTNAM = TEXT(:NDX)
         I = JTRIM (HSTNAM)
         CALL CHUTOL (I, HSTNAM)
      ELSE
         HSTNAM = 'unknown'
         END IF
C                                       If DODISK, read values from
C                                       system parameter file, otherwise
C                                       go with hard-coded values set as
C                                       above.
      IF (DODISK) THEN
C                                       Build name of system parameter
C                                       file.
         CALL ZPHFIL ('SP', 1, 0, 0, PNAME, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Open system parameter file.
         CALL ZOPEN (SPLUN, SPIND, 1, PNAME, MAP, EXCL, WAIT, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Read first and only record of
C                                       system parameter file.
         CALL ZFIO ('READ', SPLUN, SPIND, 1, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Reset integer parameters.
         NVOL = IOBLK(1)
         NTAPED = IOBLK(2)
         CRTMAX = IOBLK(3)
C                                       Set up window-size detection.
         CALL ZWINC (I)
         PRTMAX = IOBLK(4)
         NBATQS = IOBLK(5)
         MAXXPR(1) = IOBLK(6)
         MAXXPR(2) = IOBLK(7)
         CSIZPR(1) = IOBLK(8)
         CSIZPR(2) = IOBLK(9)
         NINTRN = IOBLK(10)
         KAPWRD = IOBLK(11)
         NTVDEV = IOBLK(12)
         NTKDEV = IOBLK(13)
         NTVACC = MIN (NINTRN, MAX (NTVDEV, IOBLK(64)))
         NTKACC = MIN (NINTRN, MAX (NTKDEV, IOBLK(65)))
         UCTSIZ = IOBLK(66)
         USELIM = IOBLK(67)
         NCHPRT = IOBLK(68)
         KAP2WD = IOBLK(69)
         NSHORT = IOBLK(70)
C                                       Reset DEVTAB array values.
         CALL COPY (50, IOBLK(14), DEVTAB)
C                                       Reset floating-point parameters.
         XPRDMM = ROBLK(71)
         XTKDMM = ROBLK(72)
         CALL H2CHR (20, 1, ROBLK(73), SYSNAM)
         IF (SYSNAM.EQ.'UN-REGISTERED!!!!') SYSNAM = SITNAM
         CALL RCOPY (15, ROBLK(78), TIMEDA)
         TIMESG = ROBLK(93)
         TIMEMS = ROBLK(94)
         TIMESC = ROBLK(95)
         TIMECA = ROBLK(96)
         CALL RCOPY (4, ROBLK(97), TIMEBA)
         CALL RCOPY (3, ROBLK(101), TIMEAP)
C                                       set disk table
         CALL COPY (120, IOBLK(104), DASSGN)
C                                       Graphics device
         MAXXTK(1) = IOBLK(224)
         MAXXTK(2) = IOBLK(225)
         CSIZTK(1) = IOBLK(226)
         CSIZTK(2) = IOBLK(227)
         PRDELE = IOBLK(228)
         AIPSMK = ROBLK(238)
         IF (AIPSMK.LE.0.0) AIPSMK = 20.0
         CALL COPY (NDCHD, IOBLK(257-NDCHD), DCHDUM)
         IERR = 0
C                                       catch old valuess
         IF ((KAP2WD.GT.0) .OR. (KAPWRD.LT.1024)) THEN
            KAPWRD = 132 * 1024
            IF (ADDRSZ.EQ.8) KAPWRD = 260 * 1024
            KAP2WD = 0
            END IF
C                                       Close system parameter file.
 970     CALL ZCLOSE (SPLUN, SPIND, JERR)
C                                       Disk error.  Print warning.
 980     IF (IERR.NE.0) THEN
            WRITE (ERRTER,1980)
            WRITE (ERRTER,1981)
            END IF
C                                       Check for NTAPED logical.
         CALL ZTRLOG (6, 'NAIPST', 192, TEXT, NDX, JERR)
C                                       Dynamic tape allocation
         IF ((JERR.EQ.0) .AND. (TEXT.NE.' ')) THEN
            NDX = 1
            CALL GETNUM (TEXT, 192, NDX, DP)
            IF ((DP.NE.DBLANK) .AND. (DP.GE.1D0) .AND. (DP.LE.35D0))
     *         NTAPED = NINT (DP)
            END IF
C                                       Check for NVOL logical.
         CALL ZTRLOG (4, 'NVOL', 192, TEXT, NDX, JERR)
         IF ((JERR.EQ.0) .AND. (TEXT.NE.' ')) THEN
C                                       Dynamic data disk allocation
            NDX = 1
            CALL GETNUM (TEXT, 192, NDX, DP)
            IF ((DP.NE.DBLANK) .AND. (DP.GE.1D0) .AND. (DP.LE.35D0))
     *         NVOL = NINT (DP)
C                                       Open  network sys. parm. file
            PNAME = 'NET0:NETSP'
            CALL ZTXOPN ('QRED', NETLUN, SPIND, PNAME, .FALSE., IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ZDCHIN: COULDN''T OPEN NETSP FILE, CONTINUING.'
               CALL MSGWRT (6)
               GO TO 995
               END IF
C                                       Read the whole file in.
            NENT = 0
            NEOF = .FALSE.
 981        IF (NEOF) GO TO 982
               CALL ZTXIO ('READ', NETLUN, SPIND, TEXT, IERR)
C                                       End-of-file.
               IF (IERR.EQ.2) THEN
                  IERR = 0
                  NEOF = .TRUE.
               ELSE IF (IERR.NE.0) THEN
                  MSGTXT = 'ZDCHIN: ERROR READING NETSP FILE, ' //
     *               'CONTINUING.'
                  CALL MSGWRT (6)
                  NEOF = .TRUE.
               ELSE IF ((TEXT(1:1).NE.'#') .AND. (TEXT.NE.' ')) THEN
                  NENT = NENT + 1
                  IF (NENT.GT.1024) THEN
                     MSGTXT = 'ZDCHIN: TOO MANY ENTRIES IN NETSP;'
     *                   // ' ONLY READING FIRST 1024'
                     CALL MSGWRT (6)
                     NEOF = .TRUE.
                  ELSE
                     CALL CHTRIM (TEXT, 192, TEXT, NDX)
                     NETSP(NENT) = TEXT
                     END IF
                  END IF
               GO TO 981
 982        CONTINUE
C                                       Close NETSP.
 984        CALL ZTXCLS (NETLUN, SPIND, IERR)
            DO 988 IVOL = 1,NVOL
C                                       Translate disk logical.
               DA0X = 'DA00'
               CALL ZEHEX (IVOL, 1, DA0X(4:4))
               CALL ZTRLOG (4, DA0X, 192, TEXT, NDX, IERR)
               IF ((IERR.NE.0) .OR. (TEXT.EQ.' ')) THEN
                  MSGTXT = 'ZDCHIN: ERROR TRANSLATING ' // DA0X //
     *               ', CONTINUING.'
                  CALL MSGWRT (6)
                  GO TO 988
                  END IF

C                                       Look for this disk in NETSP.
               CALL CHTRIM (TEXT, 192, TEXT, NDX)
               DO 985 IENT = 1,NENT
                  IF (NETSP(IENT)(1:NDX).EQ.TEXT(1:NDX)) GO TO 986
 985              CONTINUE
               IF (TSKNAM.EQ.'AIPS') THEN
                  MSGTXT = 'ZDCHIN: NO NETSP ENTRY FOR ' // DA0X
                  CALL MSGWRT (6)
                  END IF
               GO TO 988
C                                       Get TIMDEST value.
 986           NDX = NDX + 1
               CALL GETNUM (NETSP(IENT), 196, NDX, DP)
               IF (DP.NE.DBLANK) TIMEDA(IVOL) = DP
C                                       Get disk access values.
               DO 987 J = 1,8
                  CALL GETNUM (NETSP(IENT), 196, NDX, DP)
                  IF (DP.NE.DBLANK) DASSGN(J,IVOL) = NINT(DP)
 987              CONTINUE
 988           CONTINUE
            END IF
C                                       GNU Copyleft statement must be
C                                       in binaries also
      ELSE
         IERR = 0
         LINE = GPLBRK(1) // GPLBRK(2)
         WRITE (ERRTER,1988) LINE
         CALL ZDATE (IDATE)
C                                       Should work until 2200 :-)
         IF (IDATE(1).LT.200) IDATE(1) = IDATE(1) + 1900
         WRITE (LINE,1300) GPLDAT, IDATE(1)
         WRITE (ERRTER,1988) LINE
         DO 991 J = 1,NGNU
            LINE = GPLTXT(1,J) // GPLTXT(2,J)
            WRITE (ERRTER,1988) LINE
 991        CONTINUE
         LINE = GPLBRK(1) // GPLBRK(2)
         WRITE (ERRTER,1988) LINE
         END IF
C                                       init dynamic memory
 995  CALL ZMEMRY ('INIT', SYSNAM, IOBLK, IOBLK, IOBLK, IERR)
      PSAPOF = 0
      PSAPNW = 0
C                                       File open limit check/set
      NFTAB = NTAB1 + NTAB2 + NTAB3
      CALL ZULIM (NFTAB)
C                                       clear old tape markers
      DO 996 I = 1,35
         IF (DEVTAB(30+I).EQ.2) THEN
            DEVTAB(30+I) = 0
         ELSE
            GO TO 997
            END IF
 996     CONTINUE
C                                       use dynamic tape set DEVTAB
 997  DO 998 I = 1,NTAPED
         DEVTAB(129-I) = 2
 998     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1300 FORMAT (1X,A,I4)
 1980 FORMAT (' ZDCHIN: COULD NOT READ PARAMETER FILE')
 1981 FORMAT (' ZDCHIN: (USING MINIMUM SYSTEM CONFIGURATION)')
 1988 FORMAT (1X,A)
      END
