      PROGRAM CCMRG
C-----------------------------------------------------------------------
C! Merge CC (CLEAN components) table.
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009
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
C   Task CCMRG sorts AIPS CC tables to bring all components at the same
C   cell together, then it sums them, and finally it resorts the file
C   into the original order (by flux of the new components).
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name.
C      INCLASS        CLASS         File class.
C      INSEQ          SEQ           File sequence number.
C      INDISK         DISK          Disk volume on which file resides.
C      INVER          INVER         Input file version number
C      IN2VER         OUTVER        Output file version number
C      BADDISK        IBADD(10)     Disks to avoid for scratch.
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLASS*6, PTYPE*2, PRGM*6, ATIME*8, ADATE*12,
     *   HILINE*72
      INTEGER   CATBLK(256), USERID, SEQ, INVER, OUTVER, CNO, DISK,
     *   IRET, BUFSZ, NPARM, IBUF(1), IROUND, LUN, FIND, IERR, ID(3),
     *   IT(3), HLUN, I, INPCMP, OUTCMP
      REAL      XSEQ, XDISK, XVER, X2VER, BADD(10), CATR(256),
     *   BUFFER(8196)
      DOUBLE PRECISION CATD(64)
      HOLLERITH XNAME(3), XCLASS(2), CATH(256)
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XVER, X2VER, BADD
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (IBUF, BUFFER)
      DATA PRGM  /'CCMRG '/
      DATA LUN, HLUN /16,27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init I/O, parameters
      NPARM = 19
      CALL SETUP (PRGM, NPARM, XNAME, IBUF, IRET)
      IF (IRET.NE.0) GO TO 990
      USERID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      INVER = IROUND (XVER)
      OUTVER = IROUND (X2VER)
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
C                                       Open file and get CATBLK.
      PTYPE = '  '
      CALL MAPOPN ('WRIT', DISK, NAME, CLASS, SEQ, PTYPE, USERID,
     *   LUN, FIND, CNO, CATBLK, IBUF, IRET)
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
      CALL ZCLOSE (LUN, FIND, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 990
C                                       BADDISK
 20   DO 25 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 25      CONTINUE
C                                       Sort: to scratch (1)
      BUFSZ = 8192
      CALL CCMERG (DISK, CNO, INVER, OUTVER, INPCMP, OUTCMP, BUFSZ,
     *   BUFFER, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISK, CNO, IBUF, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 990
C                                       Version, date, time
 110  CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (HILINE,1110) TSKNAM, RLSNAM, ADATE, ATIME
      CALL HIADD (HLUN, HILINE, IBUF, IERR)
      IF (IERR.NE.0) GO TO 120
C                                       Input vers, number
      WRITE (MSGTXT,1111) INPCMP, INVER
      CALL MSGWRT (3)
      WRITE (HILINE,1112) TSKNAM, INPCMP, INVER
      CALL HIADD (HLUN, HILINE, IBUF, IERR)
      IF (IERR.NE.0) GO TO 120
C                                       Output vers, number
      WRITE (MSGTXT,1113) OUTCMP, OUTVER
      CALL MSGWRT (3)
      WRITE (HILINE,1114) TSKNAM, OUTCMP, OUTVER
      CALL HIADD (HLUN, HILINE, IBUF, IERR)
      IF (IERR.NE.0) GO TO 120
C
 120  CALL HICLOS (HLUN, T, IBUF, IERR)
C                                       Write end message
 990  CALL DIE (IRET, IBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I7,' OPENING MAP FILE')
 1010 FORMAT ('ERROR:',I7,' CLOSING MAP FILE')
 1100 FORMAT ('ERROR:',I7,' OPENING HISTORY FILE')
 1110 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Run',
     *   A12,2X,A8)
 1111 FORMAT ('Read ',I10,' CLEAN components from CC file version',I3)
 1112 FORMAT (A6,'/Read ',I10,
     *   ' CLEAN components from CC file version',I3)
 1113 FORMAT ('Wrote',I10,' CLEAN components to CC file version',I3)
 1114 FORMAT (A6,'/Wrote',I10,' CLEAN components to CC file version',
     *   I3)
      END
