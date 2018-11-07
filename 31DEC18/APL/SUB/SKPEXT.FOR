      SUBROUTINE SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *   TAPBUF, FITBLK, IERR)
C-----------------------------------------------------------------------
C! finishes reading FITS extension header, skips the extension data
C# FITS tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999
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
C  This routine will read the rest of the FITS extension file header
C  to find the END card, and then skip over a number of blocks of data.
C  Inputs:
C     DOHDR    L         T => read rest of header
C     FDVEC    I(50)     File descriptor vector for TAPIO input
C     HLUN     I         LUN of open history file.
C     INBLK    I         Number of data blocks to skip.
C  In/Out:
C     TBIND    I         Buffer pointer in FITBLK
C     ICARD    I         Current card number in FITBLK.
C     TAPBUF   I(*)      Tape I/O block.
C     FITBLK   C*2880    Work buffer - local header record
C  Output:
C     IERR     I         Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, HLUN, ICARD, INBLK, HBUFF(256),
     *   TAPBUF(*), IERR
      LOGICAL   DOHDR
      CHARACTER FITBLK*2880
C
      CHARACTER SYM*8, HILINE*72, CARD*80
      INTEGER   I, INC, ITYP, JERR, NPNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
C-----------------------------------------------------------------------
C                                       Read until we get an end card
      IF (DOHDR) THEN
         DO 10 I = 1,32000
            CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Look for END card
            INC = (ICARD - 1) * 80 + 1
            CARD = FITBLK(INC:INC+79)
C                                       Get next symbol.
            NPNT = 1
            CALL GETSYM (CARD, NPNT, SYM, ITYP)
C                                       Test for END card.
            IF (SYM.EQ.'END ') GO TO 20
            ICARD = ICARD + 1
 10         CONTINUE
C                                       Shouldn't get here.
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Advance the proper number of
C                                       records.
 20   DO 30 I = 1,INBLK
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.EQ.0) GO TO 30
         IF (IERR.EQ.4) GO TO 999
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (8)
            GO TO 999
 30      CONTINUE
C                                       Update history, print warning
      IF (EXTTYP.EQ.'BINTABLE') THEN
         WRITE (MSGTXT,1030) ITYPE, EXTVER, INBLK
      ELSE
         WRITE (MSGTXT,1031) INBLK, EXTTYP
         END IF
      CALL MSGWRT (6)
      HILINE = TSKNAM // '/ ' // MSGTXT(:64)
      CALL HIADD (HLUN, HILINE, HBUFF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SKPEXT: MORE THAN 32000 HEADER CARDS FOR THIS',
     *   ' EXTENSION.')
 1020 FORMAT ('SKPEXT: I/O ERROR ',I3,' READING UNKNOWN TYPE TABLE')
 1030 FORMAT ('Skipped ',A,' extension file',I5,' of',I5,' blocks')
 1031 FORMAT ('SKIPPED UNKNOWN EXTENSION FILE OF',I5,' BLOCKS.',
     *   ' EXTTYP = ''',A8,'''')
      END
