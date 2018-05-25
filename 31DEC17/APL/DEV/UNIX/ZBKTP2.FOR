      SUBROUTINE ZBKTP2 (ZLUN, ZBKNAM, BAKTXT, BAKREC, IVOL, ICNO,
     *   NTAPE, ZMT0N, LZMT0N, IERR)
C-----------------------------------------------------------------------
C! write a cataloged file plus extensions to BACKUP tape in BAKTP
C# Z Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C-----------------------------------------------------------------------
C   Host-dependent subroutine to write a file and its extensions to
C   tape using the hosts "backup" utility.
C   Inputs:
C      ZLUN     I       LUN for command file
C      ZBKNAM   C*(*)   Name of command file
C      BAKTXT   C*(*)   lowest file name for file listing
C      BAKREC   C*60    ?
C      IVOL     I       Disk number
C      ICNO     I       Catalog number
C      NTAPE    I       Tape drive number
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C   Output:
C      IERR     I       Error code: 0 => okay, else ZSHCMD
C   Unix version
C-----------------------------------------------------------------------
C   WRITE (ZLUN,'(''tar cvf /tmp/BAKAIP.TAR *'')')
C   NOTE#1: "tar" normally defaults to reading to EOI and then appending
C   new files. We want to create "savesets" delimited by tapemarks.
C   Therefore, we use the "c" mode of tar to suppress the read, and pass
C   the output through "dd" so that tar will not be able to rewind the
C   tape.
C   NOTE#2: another reason to pipe through "dd" is that it can reblock
C   the file. Experiments show that "dd" is much slower when reblocking,
C   and this is disappointing, but the default block copy ["bs=20b"]
C   results in lots of irregular blocks.  Therefore, we tell tar to
C   produce "1b" [512] blocks, and tell dd to reblock them to "60b"
C   [30720, 94% efficiency 6250].  We need to build a custom
C   asynchronous reblocker for maximum performance on the Convex.
C   NOTE#3: evidence suggests that the Convex Unix "dd" writes a
C   double-tapemark EOI and positions in between the marks.
C-----------------------------------------------------------------------
      INTEGER   ZLUN, IVOL, ICNO, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), BAKREC*(*), ZMT0N*(*)
C
      INTEGER   ITRIM, JERR, NP, XLEN, LZDA0N, CLEN
      CHARACTER COMMND*40, FNAME*50, GDA0N*4, CTEMP*1, CUSER*3, CCNO*3,
     *   TYPINO*2, MNAME*48, DATVER*1, TEXT*128, ZDA0N*128
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       translate logical DA0n:
      CALL ZEHEX (IVOL, 1, CTEMP)
      GDA0N = 'DA0' // CTEMP
      XLEN = LEN (ZDA0N)
      CALL ZTRLOG (4, GDA0N, XLEN, ZDA0N, LZDA0N, JERR)
      IF (JERR.NE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1000) JERR
         GO TO 990
         END IF
C                                       generate special catalog file:
      CALL ZEHEX (NPOPS, 1, CTEMP)
      FNAME = ZDA0N(1:LZDA0N) // '/BAKTP' // CTEMP // '.TXT'
      OPEN (ZLUN, FILE=FNAME, STATUS='UNKNOWN', ERR=900)
      REWIND ZLUN
      WRITE (ZLUN,1010) BAKREC
      WRITE (ZLUN,1010) HSTNAM // ' ' // SYSNAM
      CLOSE (ZLUN)
C                                       get data-version code:
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), TYPINO)
      CALL ZPHFIL (TYPINO, IVOL, ICNO, 1, MNAME, IERR)
      NP = INDEX (MNAME, TYPINO)
      NP = NP + 2
      DATVER =  MNAME(NP:NP)
C                                       generate command file:
      OPEN (ZLUN, FILE=ZBKNAM, STATUS='OLD', ERR=910)
      REWIND ZLUN
C                                       change to base directory
      TEXT = 'cd ' // ZDA0N(1:LZDA0N)
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
C                                       make special directory
      TEXT = 'mkdir BAKTP' // CTEMP // '.DIR'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
C                                       change to it
      TEXT = 'cd BAKTP' // CTEMP // '.DIR'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
C                                       link
      TEXT = 'ln '// FNAME(1:ITRIM(FNAME)) // ' ' //
     *   BAKTXT(1:ITRIM(BAKTXT))
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
      CALL ZEHEX (USER, 3, CUSER)
      CALL ZEHEX (ICNO, 3, CCNO)
      TEXT = 'ln ../??' // DATVER // CCNO // '???.' // CUSER // '?1 .'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
C                                       tar
      TEXT = 'tar cbf 1 - * | dd of=' // ZMT0N(1:LZMT0N) //
     *   ' ibs=1b obs=60b'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
C                                       cd back up to base
      WRITE (ZLUN,1010,ERR=920) 'cd ..'
C                                       clean up the files
      TEXT = 'rm -r BAKTP' // CTEMP // '.DIR ' // FNAME(1:ITRIM(FNAME))
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
      CLOSE (ZLUN)
C                                       now execute it (Bourne shell):
      COMMND = ZBKNAM
      CLEN = ITRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
      GO TO 999
C                                       Errors - can't open catalog
 900  MSGTXT = 'ZBKTP2: CAN''T OPEN ' // FNAME
      IERR = 1
      GO TO 990
C                                       Errors - can't open command
 910  MSGTXT = 'ZBKTP2: CAN''T OPEN ' // ZBKNAM
      IERR = 1
      GO TO 990
C                                       Errors - can't write command
 920  MSGTXT = 'ZBKTP2: CAN''T WRITE ' // ZBKNAM
      IERR = 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZBKTP2: CAN''T TRANSLATE DISK LOGICAL NAME, ERR',I6)
 1010 FORMAT (A)
      END
