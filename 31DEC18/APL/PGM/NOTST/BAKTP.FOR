      PROGRAM BAKTP
C-----------------------------------------------------------------------
C! write machine-dependent backup tape of 1 or more  catalog entries
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   This program will write a series of maps or UV data bases plus
C   extensions to tape using the "backup" (machine-dependent) format of
C   the host system. Each set of file+extension(s) is in a separate
C   physical file on the tape, in order to facilitate selective
C   reloading. On VMS the tape format is "BACKUP" and on Unix it is tar.
C   Inputs:
C      DOALL     R      > 0 means do every catalog entry that matches
C                       the inputs.  Else do 1st that matches.
C      INNAME    H*12   Name of input file.
C      INCLASS   H*6    Class of file.
C      INSEQ     R      sequence number of file.
C      INDISK    R      disk volume no. of file.
C      INTYPE    R      file type (UV or MA or blank).
C      OUTTAPE   R      tape number for output.
C      OPCODE    H*4    'INIT'=initialize tape, 'BACK' or ' '= do BAKTP.
C-----------------------------------------------------------------------
      INTEGER   ISCR(256), ICNO, IMAX, IERR, IVOL, INDEX, JERR, LSEQ,
     *   ZLUN, NTAPE, LZMT0N, I
      LOGICAL   FIND1, T, F, EOF
      CHARACTER ZBKNAM*30, BAKTXT*12, ZMT0N*48, CNAME*12, CCLASS*6,
     *   CTYPE*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DBTP.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       init CFILs counters
      NCFILE = 0
      NSCR = 0
      TIND = 0
C                                       Get input parameters, defaults
      CALL BAKINI (RQUICK, ISCR, ZLUN, ZBKNAM, BAKTXT, NTAPE, ZMT0N,
     *   LZMT0N, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       branch on OP:
      IF (OP.EQ.'INIT') GO TO 990
      IF ((OP.NE.'BACK') .AND. (OP.NE.' ')) THEN
         WRITE (MSGTXT,1020) OP
         CALL MSGWRT (7)
         GO TO 990
         END IF
C                                       BACK op: loop for all disks.
      FIND1 = F
      IMAX = 32767
      IF (DOALL.LE.0.0) IMAX = 1
      IF (ISEQ.LE.0) ISEQ = 0
      LSEQ = ISEQ
      IF ((ISEQ.LE.0) .AND. (DOALL.LE.0.0)) LSEQ = -1
      CALL H2CHR (12, 1, NAME, CNAME)
      CALL H2CHR (6, 1, CLASS, CCLASS)
      CALL H2CHR (2, 1, TYPE, CTYPE)
      DO 100 IVOL = IBVOL,IEVOL
C                                       Loop for all files on disk.
         ICNO = 0
         DO 80 INDEX = 1,IMAX
            ICNO = ICNO + 1
C                                       Find next data base.
            CALL H2CHR (12, 1, NAME, CNAME)
            CALL H2CHR (6, 1, CLASS, CCLASS)
            CALL H2CHR (2, 1, TYPE, CTYPE)
            CALL NXTMAP ('READ', IVOL, CNAME, CCLASS, LSEQ, CTYPE, USER,
     *         DLUN, DIND, ICNO, CATBLK, ISCR, EOF, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (EOF) GO TO 90
            CALL ZCLOSE (DLUN, DIND, IERR)
            FIND1 = T
C                                       Set DIE values for this file.
            NCFILE = 1
            FVOL(1) = IVOL
            FCNO(1) = ICNO
            FRW(1) = 0
C                                       write this one out:
            CALL BAKWRT (ZLUN, ZBKNAM, BAKTXT, USER, IVOL, ICNO,
     *         NTAPE, ZMT0N, LZMT0N, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL CATDIR ('CSTA', IVOL, ICNO, CNAME, CCLASS, LSEQ, CTYPE,
     *         USER, 'CLRD', ISCR, JERR)
            NCFILE = 0
C
 80         CONTINUE
 90      IF ((IMAX.EQ.1) .AND. (FIND1)) GO TO 900
 100     CONTINUE
C                                       Could not find file of type ...
 900  IF (.NOT.FIND1) THEN
         WRITE (MSGTXT,1900) CTYPE, USER, CNAME, CCLASS, ISEQ, IBVOL,
     *      IEVOL
         CALL MSGWRT (7)
         IERR = 1
      ELSE
         MSGTXT = 'Adding extra EOFs for safety - sorry for the delay'
         CALL MSGWRT (2)
         IERR = 0
         IF (TIND.LE.0) CALL ZTPOPN (TLUN, TIND, NTAPE, TNAME, 'WRIT',
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1910) IERR
            CALL MSGWRT (8)
         ELSE
            I = 1
            CALL ZTAPE ('BEGW', TLUN, TIND, I, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1915) IERR
               CALL MSGWRT (8)
               END IF
            END IF
         END IF
C
 990  IF (OP.NE.'INIT') THEN
         CALL ZBKTP3 (ZLUN, ZBKNAM, JERR)
         IF (IERR.EQ.0) IERR = JERR
         END IF
      IF (TIND.GT.0) THEN
         CALL ZTPCLS (TLUN, TIND, JERR)
         IF (IERR.EQ.0) IERR = JERR
         END IF
      CALL DIE (IERR, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1020 FORMAT ('ILLEGAL OPCODE ',A4,'; Legal = INIT, BACK, or blank.')
 1900 FORMAT ('NO ',A2,' FILE! USER:',I5,1X,A12,'.',A6,'.',I4,' VOLS:',
     *   I2,' -',I2)
 1910 FORMAT ('ERROR',I5,' REOPENING THE TAPE')
 1915 FORMAT ('ERROR',I5,' WRITING EOF OR REPOSITIONING TAPE')
      END
      SUBROUTINE BAKINI (RQUICK, ISCR, ZLUN, ZBKNAM, BAKTXT, NTAPE,
     *   ZMT0N, LZMT0N, IRET)
C-----------------------------------------------------------------------
C   BAKINI does the most basic inits for BAKTP.  Get the parameters,
C   restart AIPS if required, fill in defaults, init the tape I/O.
C   For the 'INIT' opcode, BAKINI expects ZBAKTP1 to do the work.
C   For the 'BACK' opcode, position tape just before EOI.
C   Outputs:
C      RQUICK  L       T -> AIPS already restarted
C      ISCR    I(256)  Scratch buffer
C      ZLUN    I       LUN to use (set in ZMKTP1)
C      ZBKNAM  C*(*)   Full name of command file
C      BAKTXT  C*(*)   Name of text file for commands
C      NTAPE   I       Tape drive unit number
C      ZMT0N   C*(*)   tape name - translattion of MT0n
C      LZMT0N  I       length of last char in ZMT0N
C      IRET    I       Return code : 0 => ok, else quit
C   Common
C      /INPARM/
C      /MORPRM/
C-----------------------------------------------------------------------
      LOGICAL   RQUICK
      INTEGER   ISCR(256), ZLUN, NTAPE, LZMT0N, IRET
      CHARACTER ZBKNAM*(*), BAKTXT*(*), ZMT0N*(*)
C
      CHARACTER PRGNAM*6
      INTEGER   NPARM, IERR
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBTP.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM, NPARM /'BAKTP ', 11/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE., ISCR)
      CALL VHDRIN
C                                       Initialize for AIPS
      CALL GTPARM (PRGNAM, NPARM, RQUICK, DOALL, ISCR, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 20
C                                       Only interactive tasks
 10   IF ((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) GO TO 20
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         IRET = 4
C                                       Restart AIPS
 20   IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
      IF (IRET.NE.0) GO TO 999
      CALL H2CHR (4, 1, OPCODE, OP)
C                                       get tape drive:
      NTAPE = NTAPE4 + 0.5
      IF (NTAPE.EQ.0) NTAPE = 1
      TLUN = 129 - NTAPE
      CALL ZPHFIL ('MT', NTAPE, 1, 1, TNAME, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
C
 30   CALL ZTPOPN (TLUN, TIND, NTAPE, TNAME, 'WRIT', IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         GO TO 990
C                                       Set some default values,
C                                       and global parameters.
 40   USER = NLUSER
      DLUN = 16
      ISEQ = SEQ4 + .5
      IBVOL = KVOL4 + .5
      IEVOL = IBVOL
      IF (IBVOL.LE.0) THEN
         IBVOL = 1
         IEVOL = NVOL
         END IF
C                                       BAKTP init:
      BAKTXT = 'BAKAIP.TXT'
      CALL ZBKTP1 (ZLUN, ZBKNAM, BAKTXT, NTAPE, ZMT0N, LZMT0N, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (OP.NE.'INIT') THEN
         WRITE (MSGTXT,1040)
         CALL MSGWRT (4)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
 995  IRET = 16
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET PARMS.  IER=',I3)
 1010 FORMAT ('TAPES NOT AVAILABLE TO BATCH AIPS  ')
 1020 FORMAT ('COULD NOT GET PHYSICAL NAME OF TAPE.  IER=',I4)
 1030 FORMAT ('COULD NOT OPEN TAPE.  IER=',I4)
 1040 FORMAT ('V-Cno User Rlsnam  Date-bak Date-map Ty Name',8X,
     *   ' Class   Seq')
      END
      SUBROUTINE BAKWRT (ZLUN, ZBKNAM, BAKTXT, USID, IVOL, ICNO, NTAPE,
     *   ZMT0N, LZMT0N, IERR)
C-----------------------------------------------------------------------
C   Write it out!
C   Inputs:
C      ZLUN    I       LUN to use (set in ZMKTP1)
C      ZBKNAM  C*(*)   Full name of command file
C      BAKTXT  C*(*)   Name of text file for commands
C      USID    I       User number
C      IVOL    I       Disk number
C      ICNO    I       Catalog entry number
C      NTAPE   I       Tape drive unit number
C      ZMT0N   C*(*)   tape name - translattion of MT0n
C      LZMT0N  I       length of last char in ZMT0N
C   Outputs:
C      IERR     I
C-----------------------------------------------------------------------
      INTEGER   ZLUN, USID, IVOL, ICNO, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), ZMT0N*(*)
C
      CHARACTER BAKREC*60
      INTEGER   IDATE(3)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTP.INC'
C-----------------------------------------------------------------------
C                                       encode directory string:
      CALL ZDATE (IDATE)
      IDATE(1) = MOD (IDATE(1), 100)
      WRITE (BAKREC,1000) USID, RLSNAM, IDATE(3), IDATE(2), IDATE(1),
     *   CATBLK(KIIMS)
      CALL H2CHR (8, 1, CATH(KHDMP), BAKREC(23:30))
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), BAKREC(32:33))
      CALL H2CHR (12, KHIMNO, CATH(KHIMN),BAKREC(35:46))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), BAKREC(48:53))
      WRITE (MSGTXT,1005) IVOL, ICNO
      MSGTXT(7:) = BAKREC(:58)
      CALL MSGWRT (2)
C                                       do machine-dep stuff:
      CALL ZBKTP2 (ZLUN, ZBKNAM, BAKTXT, BAKREC, IVOL, ICNO, NTAPE,
     *   ZMT0N, LZMT0N, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4,1X,A7,1X,I2.2,'/',I2.2,'/',I2.2,1X,8X,1X,2X,1X,
     *   12X,'.',6X,'.',I4.4)
 1005 FORMAT (I1,'-',I3.3)
      END
