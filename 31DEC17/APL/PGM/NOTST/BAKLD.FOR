      PROGRAM BAKLD
C-----------------------------------------------------------------------
C! read catalog entries from machine-dependent bakup tapes from BAKTP
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
C   BAKLD is the task to read files written to tape by task BAKTP in a
C   host-dependent format (BACKUP under VMS, "tar" under Unix).  BAKLD
C   has two modes of operation: 'LOAD' and 'PRNT'. The LOAD mode
C   selectively reloads AIPS files from tape to disk. The PRNT mode
C   produces a directory of a tape, which can be used to select files
C   for LOADing.
C   AIPS input parameters:
C      INTAPE     R      Input tape unit #
C      OUTNAME    H*12   Image name (name)
C      OUTCLASS   H*6    Image name (class)
C      OUTSEQ     R      Image name (seq #) (< 0 => tape value)
C      OUTDISK    R      Output disk unit #
C      NCOUNT     R      Number of images to load.
C      NFILES     R      Number of files to skip first (<0 back, 0 to
C                        start of current file)
C      OPCODE     h*4    'LOAD', 'PRNT', or ' '
C      DOCRT      R      'PRNT' listing to terminal, or printer?
C-----------------------------------------------------------------------
      INTEGER   IRET, ISCR(256), JERR, I, N, IROUND, K, ERRLIM, ERRCNT
      CHARACTER OP*4
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBKL.INC'
C-----------------------------------------------------------------------
      TIND = 0
      CALL BKLINI (RQUICK, ISCR, IRET)
      IF (IRET.NE.0) GO TO 995
      ERRLIM = 3
      ERRCNT = 0
C                                       Process the tape:
      DO 150 K = 1,2
C                                       skip files first:
         IF (K.EQ.1) THEN
            N = IROUND (NFILES)
            IF (N.LT.0) N = 0
            OP = 'SKIP'
            ERRLIM = ERRLIM - N
C                                       then process files:
         ELSE
            N = NCOUNS + .05
            IF (N.LE.0) N = 1
            CALL H2CHR (4, 1, OPCODE, OP)
            END IF
C                                       tape loop:
         IF (N.GT.0) THEN
            DO 140 I = 1,N
C                                       virgin copy of parms:
 130           CALL ZBKLD2 (OP, IRET)
               IF (IRET.EQ.999) THEN
                  ERRCNT = ERRCNT + 1
                  IF ((K.NE.2) .OR. (ERRCNT.GT.ERRLIM)) GO TO 160
                     MSGTXT = 'Try it (' // OP // ') again'
                     CALL MSGWRT (6)
                     GO TO 130
               ELSE IF (IRET.NE.0) THEN
                  GO TO 995
               ELSE
                  IF (K.EQ.2) ERRLIM = 0
                  END IF
 140           CONTINUE
            END IF
 150     CONTINUE
C                                       Clean up
 160  CALL ZBKLD3 (IRET)
C
 995  IF (TIND.GT.0) CALL ZTPCLS (TLUN, TIND, JERR)
      CALL DIE (IRET, ISCR)
C
 999  STOP
      END
      SUBROUTINE BKLINI (RQUICK, ISCR, IRET)
C-----------------------------------------------------------------------
C   BKLINI does the most basic inits for BAKLD.  Get the parameters,
C   restart AIPS if required, fill in defaults, init the tape I/O.
C   Outputs:
C      RQUICK  L         T -> AIPS already restarted
C      ISCR    I(256)    Scratch buffer
C      IRET    I         Return code : 0 => ok, else quit
C   Common
C      /INPARM/
C      /MORPRM/
C-----------------------------------------------------------------------
      INTEGER   ISCR(256), IRET
      LOGICAL   RQUICK
C
      CHARACTER PRGNAM*6, TNAME*48, OP*4
      INTEGER   IERR
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBKL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'BAKLD '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE., ISCR)
      CALL VHDRIN
C                                       Initialize for AIPS
      NPARM = 11
      CALL GTPARM (PRGNAM, NPARM, RQUICK, NTAPE4, ISCR, IRET)
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
C                                       get tape drive:
      NTAPE = NTAPE4 + 0.5
      IF (NTAPE.EQ.0) NTAPE = 1
      TLUN = 129 - NTAPE
      CALL ZPHFIL ('MT', NTAPE, 1, 1, TNAME, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
C
 30   CALL ZTPOPN (TLUN, TIND, NTAPE, TNAME, 'READ', IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         GO TO 990
C                                       Set some default values,
C                                       and global parameters.
 40   USER = NLUSER
C                                       verify OPCODE:
      CALL H2CHR (4, 1, OPCODE, OP)
      IF (OP.EQ.' ') OP = 'LOAD'
      IF ((OP.EQ.'LOAD') .OR. (OP.EQ.'PRNT')) GO TO 50
         WRITE (MSGTXT,1040) OP
         GO TO 990
 50   CALL CHR2H (4, OP, 1, OPCODE)
      WRITE (MSGTXT,1050)
      CALL MSGWRT (6)
C                                       BAKLD init:
      BAKTXT = 'BAKAIP.TXT'
      CALL ZBKLD1 (IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
      IRET = 16
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET PARMS.  IER=',I3)
 1010 FORMAT ('TAPES NOT AVAILABLE TO BATCH AIPS  ')
 1020 FORMAT ('COULD NOT GET PHYSICAL NAME OF TAPE.  IER=',I4)
 1030 FORMAT ('COULD NOT OPEN TAPE.  IER=',I4)
 1040 FORMAT ('OPCODE=',A4,' ILLEGAL; Legal = LOAD or PRNT')
 1050 FORMAT (4X,1X,'User',1X,'Rlsnam ',1X,'Bak-date',1X,'Map-date',
     *   1X,'TY',1X,'Name        ',1X,'Class ',1X,'Seq ')
      END
