      SUBROUTINE NXTMAP (OP, IVOL, NAMEIN, CLASIN, SEQIN, TYPIN, USID,
     *   LUN, IND, CNO, CATBLK, WBUFF, EOF, IERR)
C-----------------------------------------------------------------------
C! opens next catalog entry matching the input parameters
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   NXTMAP finds a map in the catalog starting at a given slot and
C   then opens the map file marking the catalog entry for the
C   desired type of operation.
C   Inputs:
C      OP      C*4     Operation: READ, WRIT, or INIT where INIT is
C                      for known creation processes (it ignores
C                      current file status & leaves it unchanged)
C      NAMEIN  C*12    Image name (name)
C      CLASIN  C*6     Image name (class)
C      SEQIN   I       Image name (seq.#): 0 => ANY HERE!
C                        -1 => highest
C      TYPIN   C*2     Physical type of file
C      USID    I       User identification #
C      LUN     I       Logical unit # to use
C   In/Out:
C      IVOL    I       Input disk unit: 0 in -> any; out = actual
C      CNO     I       Starting catalog slot to look for map, returns
C                      the catalog slot containing map.
C   Outputs:
C      IND     I       FTAB pointer
C      CATBLK  I(256)  Buffer containing current catalog block
C      WBUFF   I(256)  Working buffer for CATIO and CATDIR
C      EOF     L       T if end of catalog reached on search else F.
C      IERR    I       Error output
C                       0 = OK
C                       1 = Warning, file in WRITE use
C                       2 = Can't open WRIT because file busy
C                       3 = File not found
C                       4 = Catalog i/o error
C                       5 = Illegal OP code
C                       6 = Can't open file
C-----------------------------------------------------------------------
      INTEGER   IVOL, SEQIN, LUN, IND, USID, CATBLK(256), IERR,
     *   WBUFF(256)
      CHARACTER OP*4, NAMEIN*(*), CLASIN*(*), TYPIN*2
      LOGICAL   EOF
C
      CHARACTER OPER*4, CNAME*12, CCLAS*6, STAT*4, UNCODE(3)*4,
     *   STCODE(3)*4, MNAME*48, TYPINO*2, FIND*4, OPCODE(3)*4,
     *   SEARCH*4
      INTEGER   IOP, NOP, OPERR, CNO, JER, IER, I, DUM
      LOGICAL   T, F, EXCL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA DUM, OPERR /1, 5/
      DATA T, F /.TRUE., .FALSE./
      DATA FIND, SEARCH /'SRNN','SRNH'/
      DATA STCODE /'READ', 'WRIT', 'REST'/
      DATA UNCODE /'CLRD', 'CLWR', 'REST'/
      DATA NOP, OPCODE /3, 'READ', 'WRIT', 'INIT'/
C-----------------------------------------------------------------------
C                                       get op code
      IERR = 0
      JER = 4
      EOF = F
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 20
 10      CONTINUE
      IERR = OPERR
      GO TO 999
C                                       Find and open map
 20   MSGSUP = 32000
      OPER = FIND
      IF (SEQIN.LT.0) OPER = SEARCH
      CALL CATDIR (OPER, IVOL, CNO, NAMEIN, CLASIN, SEQIN, TYPIN, USID,
     *   STAT, WBUFF, IER)
      MSGSUP = 0
      IF (IER.EQ.0) GO TO 30
      IF ((IER.EQ.2) .OR. (IER.EQ.5)) GO TO 970
         WRITE (MSGTXT,1020) NAMEIN, CLASIN, SEQIN, TYPIN, USID
         CALL MSGWRT (7)
         IERR = 3
         GO TO 999
C                                       read catblock: set end status
 30   CALL CATIO ('READ', IVOL, CNO, CATBLK, STCODE(IOP), WBUFF, JER)
      I = KHIMN
      CALL H2CHR (12, KHIMNO, CATBLK(KHIMN), CNAME)
      CALL H2CHR (12, KHIMCO, CATBLK(KHIMC), CCLAS)
      IF (JER.EQ.0) GO TO 50
C                                       Catalog i/o errors
         IF (JER.GT.3) GO TO 40
            IERR = 4
            WRITE (MSGTXT,1030) JER
            GO TO 980
 40      IF (IOP.EQ.3) GO TO 50
C                                       For READ now an error
C                                       For write on busy real error
         IERR = 2
         WRITE (MSGTXT,1040) CNAME, CCLAS, SEQIN
         GO TO 980
C                                       Open the file
 50   CALL H2CHR (2, KHPTYO, CATBLK(KHPTY), TYPINO)
      IF (TYPINO.EQ.'  ') TYPINO = TYPIN
      CALL ZPHFIL (TYPINO, IVOL, CNO, 1, MNAME, IER)
      EXCL = IOP.NE.1
      CALL ZOPEN (LUN, IND, IVOL, MNAME, T, EXCL, T, IER)
      IF (IER.EQ.0) GO TO 999
         IERR = 6
         WRITE (MSGTXT,1050) CNAME, CCLAS, SEQIN, IER
         GO TO 980
C-----------------------------------------------------------------------
C                                       End of file.
 970  EOF = T
      GO TO 999
C                                       errors
 980  CALL MSGWRT (8)
      CNAME = ' '
      CCLAS = ' '
      CALL CATDIR ('CSTA', IVOL, CNO, CNAME, CCLAS, DUM, TYPINO, DUM,
     *   UNCODE(IOP), CATBLK, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A12,'.',A6,'.',I4,' ',A2,I5,' CAT PROBLEM')
 1030 FORMAT ('NXTMAP: CATALOG I/O ERROR =',I5)
 1040 FORMAT ('NXTMAP: ERROR ',A12,'.',A6,'.',I4,' TOO BUSY')
 1050 FORMAT ('CANT OPEN ',A12,'.',A6,'.',I4,' IER=',I4)
      END
