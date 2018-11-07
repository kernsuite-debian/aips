      PROGRAM MOVE
C-----------------------------------------------------------------------
C! Task to copy or move data from one user to another
C# Map-util Catalog Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2013, 2015, 2017
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
C   MOVE will copy or move an image or uv data set from one user to
C   another.  It copies any tables and other extensions found.
C   Inputs:  (from AIPS)
C      INNAME     (3) the entry name for the source file.  ' ' => the
C                     first match consistent with the other parameters
C                     is used.
C      INCLASS    (2) the class of the source file.  ' ' => the first
C                     match consistent with the other parameters is
C                     used.
C      INSEQ          the sequence number of the source file.  If 0, the
C                     first match consistent with the other parameters
C                     is used.
C      INDISK         the disk volume number of the source file. If 0,
C                     all disks are searched and the first match found
C                     is used.
C      USERID         User number for output file.  NO default.
C      OUTNAME    (3) the name of the new file.  ' ' => INNAME
C      OUTCLASS   (2) the class of the new file, ' ' => INCLASS
C      OUTSEQ         the sequence number for the new file.  0 => INSEQ,
C                     -1 => the first sequence number that will produce
C                     a unique file specification is used.
C      OUTDISK        the disk volume number for the new file.  If zero
C                     the new file will be created where there is room.
C      OPCODE         'MOVE' => rename files, else copy them
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAMIN*12, CLSIN*6, NAMOUT*12, CLSOUT*6,
     *   TYPIN*2, STAT*4, OPCODE*4, XTYPE*2, INFIL*48, OUTFIL*48
      HOLLERITH XNAMIN(3), XCLSIN(2), XNMOUT(3), XCLOUT(2), XOPCOD
      REAL      PRUSER, XDSKIN, XDSKOU, XSEQIN, XSEQOU
      INTEGER   SCRTCH(256), I, NUSER, LUSER, SEQIN, SEQOUT, DSKIN,
     *   DSKOUT, IROUND, CNOI, CNOO, ISEQ, LVOL, NVER, INPRMS, IERR,
     *   IRETCD, HLUN, IVER
      LOGICAL   ALLDSK, QUICK, IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, XSEQIN, XDSKIN, PRUSER, XNMOUT,
     *   XCLOUT, XSEQOU, XDSKOU, XOPCOD
      DATA PRGNAM /'MOVE '/
      DATA HLUN /27/
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
      INPRMS = 16
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, SCRTCH, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C
      IF (QUICK) CALL RELPOP (IRETCD, CATBLK, IERR)
      IF (IRETCD.NE.0) GO TO 995
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNMOUT, NAMOUT)
      CALL H2CHR (6, 1, XCLOUT, CLSOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IF ((OPCODE.NE.'MOVE') .AND. (OPCODE.NE.'DUPL')) OPCODE = 'COPI'
C                                       Set initial values.
      DSKIN = IROUND (XDSKIN)
      SEQIN = IROUND (XSEQIN)
      DSKOUT = IROUND (XDSKOU)
      SEQOUT = IROUND (XSEQOU)
      NUSER = ABS (PRUSER) + .01
      LUSER = NLUSER
      IF (NUSER.EQ.LUSER) THEN
         MSGTXT = 'WARNING: you are copying/moving to yourself'
         CALL MSGWRT (6)
         END IF
C                                       Open source file.
      TYPIN = ' '
      CNOI = 1
      CALL CATDIR ('SRCH', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *   LUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMIN, CLSIN, SEQIN, DSKIN, LUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DSKIN, CNOI, CATBLK, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, LUSER, 'READING HEADER'
         GO TO 990
         END IF
C                                       Build new file cat name.
      IF (NAMOUT.EQ.' ') NAMOUT = NAMIN
      IF (CLSOUT.EQ.' ') CLSOUT = CLSIN
      IF (SEQOUT.LT.0) SEQOUT = SEQIN
      IF (DSKOUT.LE.0) DSKOUT = DSKIN
      IF ((OPCODE.EQ.'MOVE') .AND. (DSKOUT.NE.DSKIN)) THEN
         MSGTXT = 'OPCODE MOVE REQUIRES INDISK=OUTDISK; CHANGE TO COPY'
         CALL MSGWRT (7)
         OPCODE = 'COPI'
         END IF
      ALLDSK = DSKOUT.LE.0
C                                       find highest seq number
      IF (SEQOUT.EQ.0) THEN
         CNOO = 1
         LVOL = 0
         ISEQ = 0
         XTYPE = ' '
         NLUSER = NUSER
         CALL CATDIR ('SRCH', LVOL, CNOO, NAMOUT, CLSOUT, ISEQ, XTYPE,
     *      NUSER, STAT, SCRTCH, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.5)) THEN
            WRITE (MSGTXT,1015) IERR, NUSER, 'GETTING HIGHEST SEQ #'
            GO TO 990
            END IF
         IF (IERR.EQ.5) ISEQ = 0
         SEQOUT = ISEQ + 1
         END IF
C                                       Set header values needed
C                                       by MCREAT.
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      CATBLK(KIIMU) = NUSER
C                                       is name unique
      CNOO = 1
      XTYPE = ' '
      LVOL = 0
      IF (OPCODE.EQ.'DUPL') LVOL = DSKOUT
      NLUSER = NUSER
      CALL CATDIR ('SRNN', LVOL, CNOO, NAMOUT, CLSOUT, SEQOUT, XTYPE,
     *   NUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.5) THEN
         IF (IERR.EQ.0) THEN
            WRITE (MSGTXT,1020) NAMOUT, CLSOUT, SEQOUT, LVOL
            GO TO 990
         ELSE
            WRITE (MSGTXT,1015) IERR, NUSER, 'CHECKING NAME'
            GO TO 990
            END IF
         END IF
      IF (ALLDSK) DSKOUT = NVOL + 1
C                                       Is it allowed here
 25   IF (ALLDSK) THEN
         DSKOUT = DSKOUT - 1
         IF (DSKOUT.LT.1) THEN
            WRITE (MSGTXT,1025) NAMOUT, CLSOUT, SEQOUT
            GO TO 990
            END IF
         END IF
      IF (.NOT.IAMOK(DSKOUT,TYPIN)) THEN
         IF ((ALLDSK) .AND. (DSKOUT.GT.1)) GO TO 25
         WRITE (MSGTXT,1026) NAMOUT, CLSOUT, SEQOUT, DSKOUT
         GO TO 990
         END IF
C                                       create the slot
      CNOO = 1
      CALL CATDIR ('OPEN', DSKOUT, CNOO, NAMOUT, CLSOUT, SEQOUT, TYPIN,
     *   NUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IF ((ALLDSK) .AND. (DSKOUT.GT.1)) GO TO 25
         WRITE (MSGTXT,1015) IERR, NUSER, 'CREATING CATALOG ENTRY'
         GO TO 990
         END IF
C                                       copy the main file
      NLUSER = LUSER
      CALL DOIT (OPCODE, TYPIN, DSKIN, DSKOUT, CNOI, CNOO, 1, LUSER,
     *   NUSER, IERR)
      IF (IERR.NE.0) THEN
         IF ((IERR.EQ.2) .AND. (ALLDSK) .AND. (OPCODE.NE.'MOVE'))
     *      GO TO 25
         IF (IERR.EQ.1) THEN
            WRITE (MSGTXT,1030) NAMIN, CLSIN, SEQIN
         ELSE
            WRITE (MSGTXT,1031) IERR, 'MAIN DATA FILE'
            END IF
         CALL MSGWRT (8)
         NLUSER = NUSER
         CALL CATDIR ('CLOS', DSKOUT, CNOO, NAMOUT, CLSOUT, SEQOUT,
     *      TYPIN, NUSER, STAT, SCRTCH, IERR)
         NLUSER = LUSER
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *      LUSER, STAT, SCRTCH, IERR)
         GO TO 995
         END IF
C                                       copy header file
      NLUSER = LUSER
      CALL DOIT (OPCODE, 'CB', DSKIN, DSKOUT, CNOI, CNOO, 1, LUSER,
     *   NUSER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1031) IERR, 'HEADER FILE'
         CALL MSGWRT (8)
         NLUSER = NUSER
         CALL ZPHFIL (TYPIN, DSKOUT, CNOO, 1, OUTFIL, IERR)
         IF (OPCODE.EQ.'MOVE') THEN
            NLUSER = LUSER
            CALL ZPHFIL (TYPIN, DSKIN, CNOI, 1, INFIL, IERR)
            CALL ZRENAM (DSKIN, OUTFIL, INFIL, IERR)
         ELSE
            CALL ZDESTR (DSKOUT, OUTFIL, IERR)
            END IF
         NLUSER = NUSER
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DSKOUT, CNOO, NAMOUT, CLSOUT, SEQOUT,
     *      TYPIN, NUSER, STAT, SCRTCH, IERR)
         CALL CATDIR ('CLOS', DSKOUT, CNOO, NAMOUT, CLSOUT, SEQOUT,
     *      TYPIN, NUSER, STAT, SCRTCH, IERR)
         NLUSER = LUSER
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *      LUSER, STAT, SCRTCH, IERR)
         GO TO 995
         END IF
C                                       extension files
      CALL FXHDEX (CATBLK)
      DO 50 I = 1,KIEXTN
         NVER = CATBLK(KIVER+I-1)
         IF (NVER.GT.0) THEN
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), XTYPE)
            DO 40 IVER = 1,NVER
               CALL DOIT (OPCODE, XTYPE, DSKIN, DSKOUT, CNOI, CNOO,
     *            IVER, LUSER, NUSER, IERR)
               IF (IERR.GT.1) THEN
                  WRITE (MSGTXT,1035) IERR, XTYPE, IVER
                  CALL MSGWRT (7)
                  END IF
 40            CONTINUE
            END IF
 50      CONTINUE
C                                       Update header file
      NLUSER = NUSER
      CALL CATIO ('UPDT', DSKOUT, CNOO, CATBLK, 'CLWR', SCRTCH, IERR)
      NLUSER = LUSER
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, NUSER, 'UPDATING CATALOG HEADER'
         CALL MSGWRT (7)
         END IF
C                                       Tell user
      WRITE (MSGTXT,1050) OPCODE, NAMIN, CLSIN, SEQIN, DSKIN, LUSER
      CALL MSGWRT (5)
      WRITE (MSGTXT,1051) NAMOUT, CLSOUT, SEQOUT, DSKOUT, NUSER
      CALL MSGWRT (5)
C                                       Initialize HITAB
      IRETCD = 0
      CALL HIINIT (3)
C                                       open HI file
      NLUSER = NUSER
      CALL HIOPEN (HLUN, DSKOUT, CNOO, SCRTCH, IERR)
      NLUSER = LUSER
      IF (IERR.NE.0) GO TO 95
C                                       Add MOVE history.
      NLUSER = NUSER
      CALL HENCO1 (PRGNAM, NAMIN, CLSIN, SEQIN, DSKIN, HLUN, SCRTCH,
     *   IERR)
      NLUSER = LUSER
      IF (IERR.NE.0) GO TO 90
      NLUSER = NUSER
      CALL HENCOO (PRGNAM, NAMOUT, CLSOUT, SEQOUT, DSKOUT, HLUN, SCRTCH,
     *   IERR)
      NLUSER = LUSER
 90   CALL HICLOS (HLUN, .TRUE., SCRTCH, IERR)
 95   NLUSER = LUSER
      STAT = 'CLRD'
      CALL CATDIR ('CSTA', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *   LUSER, STAT, SCRTCH, IERR)
      IF (OPCODE.EQ.'MOVE') THEN
         MSGSUP = 32000
         CALL CATDIR ('CLOS', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *      LUSER, STAT, SCRTCH, IERR)
         MSGSUP = 0
         END IF
      GO TO 995
C                                       error message
 990  NLUSER = LUSER
      CALL MSGWRT (8)
      STAT = 'CLRD'
      CALL CATDIR ('CSTA', DSKIN, CNOI, NAMIN, CLSIN, SEQIN, TYPIN,
     *   LUSER, STAT, SCRTCH, IERR)
C                                       Release AIPS if wait state.
 995  CALL DIETSK (IRETCD, QUICK, CATBLK)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1015 FORMAT ('ERROR',I4,' USER',I6,1X,A)
 1020 FORMAT ('DUPLICATE NAME ',2(A,'.'),I4,' ON DISK',I3)
 1025 FORMAT ('UNABLE TO CREATE SLOT FOR ',2(A,'.'),I4,' ANYWHERE')
 1026 FORMAT (2(A,'.'),I4,' PROHIBITED ON DISK',I3)
 1030 FORMAT (2(A,'.'),I4,' MISSING MAIN DATA FILE')
 1031 FORMAT ('DOIT ERROR',I2,' MOVING/COPYING ',A)
 1035 FORMAT ('DOIT ERROR',I2,' ON ',A,' VERS',I4)
 1050 FORMAT (A4,'ed ',2(A,'.'),I5,'  disk',I3,'  user',I5)
 1051 FORMAT ('to',5X,2(A,'.'),I5,'  disk',I3,'  user',I5)
      END
      SUBROUTINE DOIT (OPC, PTYPE, INDISK, OUTDSK, INCNO, OUTCNO, VER,
     *   INU, OUTU, IRET)
C-----------------------------------------------------------------------
C   actual do the move or copy of a single file
C   Inputs:
C      OPC      C*4   OPeration: 'MOVE' or copy
C      PTYPE    C*2   file type
C      INDISK   I     input disk
C      OUTDSK   I     output disk
C      INCNO    I     input catalog number
C      OUTCNO   I     output catalog number
C      VER      I     file version number
C      INU      I     input user number
C      OUTU     I     output user number
C   Outputs:
C      IRET     I     Error code: 0 okay, 1 input does not exist,
C                        2 file create fails, 3 input IO error,
C                        4 output I/O error
C-----------------------------------------------------------------------
      CHARACTER OPC*4, PTYPE*2
      INTEGER   INDISK, OUTDSK, INCNO, OUTCNO, VER, INU, OUTU, IRET
C
      INTEGER   BUFSZ
      PARAMETER (BUFSZ=8388608)
      CHARACTER INFIL*48, OUTFIL*48
      INTEGER   IERR, SIZE, INLUN, OUTLUN, ININD, OUTIND, NREC, IREC,
     *   ASIZE, LREC, NBYTES, BIGBUF(BUFSZ+8192), BLKNO, IBUFF
      INCLUDE 'INCS:DMSG.INC'
      DATA INLUN, OUTLUN /17,18/
C-----------------------------------------------------------------------
C                                       File names
      NLUSER = OUTU
      CALL ZPHFIL (PTYPE, OUTDSK, OUTCNO, VER, OUTFIL, IERR)
      NLUSER = INU
      CALL ZPHFIL (PTYPE, INDISK, INCNO, VER, INFIL, IERR)
      IRET = 1
C                                       Input exists and its size
      MSGSUP = 32000
      CALL ZEXIST (INDISK, INFIL, SIZE, IERR)
      MSGSUP = 0
      IF (IERR.NE.0) GO TO 999
      IF (PTYPE.EQ.'CB') THEN
         MSGSUP = 32000
         NLUSER = OUTU
         CALL ZDESTR (OUTDSK, OUTFIL, IERR)
         NLUSER = INU
         MSGSUP = 0
         END IF
C                                       Rename file
      IRET = 2
      IF (OPC.EQ.'MOVE') THEN
         CALL ZRENAM (INDISK, INFIL, OUTFIL, IERR)
         IF (IERR.EQ.0) THEN
            IRET = 0
            WRITE (MSGTXT,1000) PTYPE, VER
            CALL MSGWRT (3)
            END IF
C                                       Create file
      ELSE
         NLUSER = OUTU
         CALL ZCREAT (OUTDSK, OUTFIL, SIZE, .FALSE., ASIZE, IERR)
         NLUSER = INU
         IF (IERR.NE.0) GO TO 999
C                                       Open files
         CALL ZOPEN (INLUN, ININD, INDISK, INFIL, .TRUE., .FALSE.,
     *      .TRUE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            GO TO 900
            END IF
         NLUSER = OUTU
         CALL ZOPEN (OUTLUN, OUTIND, OUTDSK, OUTFIL, .TRUE., .TRUE.,
     *      .TRUE., IERR)
         NLUSER = INU
         IF (IERR.NE.0) THEN
            IRET = 4
            GO TO 900
            END IF
C                                       Copy file
         LREC = BUFSZ / 256
         NREC = SIZE / LREC
         NBYTES = LREC * 512
         BLKNO = 1
         IBUFF = 1
C                                       full buffers
         DO 10 IREC = 1,NREC
            CALL ZMIO ('READ', INLUN, ININD, BLKNO, NBYTES, BIGBUF,
     *         IBUFF, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (INLUN, ININD, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 3
               GO TO 900
               END IF
            CALL ZMIO ('WRIT', OUTLUN, OUTIND, BLKNO, NBYTES, BIGBUF,
     *         IBUFF, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (OUTLUN, OUTIND, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 4
               GO TO 900
               END IF
            BLKNO = BLKNO + LREC
 10         CONTINUE
         IREC = SIZE - (NREC * LREC)
         IF (IREC.GT.0) THEN
            NBYTES = 512 * IREC
            CALL ZMIO ('READ', INLUN, ININD, BLKNO, NBYTES, BIGBUF,
     *         IBUFF, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (INLUN, ININD, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 3
               GO TO 900
               END IF
            CALL ZMIO ('WRIT', OUTLUN, OUTIND, BLKNO, NBYTES, BIGBUF,
     *         IBUFF, IERR)
            IF (IERR.EQ.0) CALL ZWAIT (OUTLUN, OUTIND, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 4
               GO TO 900
               END IF
            END IF
C                                       Close files
         CALL ZCLOSE (INLUN, ININD, IERR)
         CALL ZCLOSE (OUTLUN, OUTIND, IERR)
         WRITE (MSGTXT,1010) PTYPE, VER, SIZE
         CALL MSGWRT (3)
         END IF
      IRET = 0
      GO TO 999
C                                       Delete on error
 900  CALL ZDESTR (OUTDSK, OUTFIL, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Moved ',A2,' file version',I4)
 1010 FORMAT ('Copied ',A2,' file version',I5,' having',I9,' blocks')
      END

