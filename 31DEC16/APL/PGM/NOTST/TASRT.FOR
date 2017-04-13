      PROGRAM TASRT
C-----------------------------------------------------------------------
C! Sorts extention tables to a user specified order
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009, 2011
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
C   Task TASRT Sorts AIPS extension tables.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name.
C      INCLASS        CLASS         File class.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volumn on which file resides.
C      INEXT          TYPE          Table type (e.g. 'CC')
C      INVER          INVER         Input file version number
C      IN2VER         OUTVER        Output file version number
C      CPARM          KEY,FKEY      Keys & weights.
C                                   1,2=> col. & wt pri. key 1.
C                                   3,4=> col. & wt sec. key 1.
C                                   5,6=> col. & wt pri. key 2.
C                                   7,8=> col. & wt sec. key 2.
C      BADDISK        IBADD(10)     Disks to aviod for scratch.
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLASS*6, PTYPE*2, PRGM*6, TYPE*2
      HOLLERITH XNAME(3), XCLASS(2), XTYP
      INTEGER   DISK, SEQ, USERID, INVER, OUTVER, KEY(2,2), NPARM, LUN,
     *   FIND, CNO,IROUND, IRET, TABUFF(512), I, CATBLK(256),
     *   BUFFER(256), KEYSUB(2,2)
      REAL   FKEY(2,2), CATR(256), XSEQ, XDISK, XVER, X2VER, CPARM(10),
     *   BADD(10)
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XTYP, XVER, X2VER,
     *   CPARM, BADD
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA PRGM  /'TASRT '/
      DATA LUN /16/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Init I/O, parameters
      NPARM = 30
      CALL SETUP (PRGM, NPARM, XNAME, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Set file info
      USERID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (2, 1, XTYP, TYPE)
      PTYPE = '  '
      INVER = IROUND (XVER)
      OUTVER = IROUND (X2VER)
C                                       Open file and get CATBLK.
      CALL MAPOPN ('WRIT', DISK, NAME, CLASS, SEQ, PTYPE, USERID, LUN,
     *   FIND, CNO, CATBLK, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 990
C                                       Mark in /CFILES/
 10   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
C                                       Close file, leave marked.
      CALL ZCLOSE (LUN, FIND, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         CALL MSGWRT (6)
         GO TO 990
C                                       Get KEY and FKEY
 20   KEY(1,1) = IROUND (CPARM(1))
      KEY(2,1) = IROUND (CPARM(3))
      KEY(1,2) = IROUND (CPARM(5))
      KEY(2,2) = IROUND (CPARM(7))
      FKEY(1,1) = CPARM(2)
      FKEY(2,1) = CPARM(4)
      FKEY(1,2) = CPARM(6)
      FKEY(2,2) = CPARM(8)
C                                       BADDISK
      DO 50 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 50      CONTINUE
C                                       Sort
      CALL TABSRT (DISK, CNO, TYPE, INVER, OUTVER, KEY, KEYSUB, FKEY,
     *   TABUFF, CATBLK, IRET)
C                                       Write end message
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,' OPENING FILE')
 1010 FORMAT ('ERROR:',I7,' CLOSING FILE')
      END
