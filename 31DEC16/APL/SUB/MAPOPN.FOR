      SUBROUTINE MAPOPN (OP, IVOL, NAMEIN, CLASIN, SEQIN, TYPIN, USID,
     *   LUN, IND, CNO, CATBLK, WBUFF, IERR)
C-----------------------------------------------------------------------
C! open file pointed to by catalog entry and mark the entry busy
C# Catalog IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   MAPOPN opens a map file marking the catalog entry for the desired
C   type of operation.
C   Inputs:
C      OP      C*4     Operation: READ, WRIT, or INIT where INIT is for
C                      known creation processes (it ignores current file
C                      status & leaves it unchanged).  Also: HDWR for
C                      use when the header is being changed, but the
C                      data are to be read only.
C      LUN     I       Logical unit # to use
C   In/out:
C      NAMEIN  C*12    Image name (name)
C      CLASIN  C*6     Image name (class)
C      SEQIN   I       Image name (seq.#)
C      USID    I       User identification #
C      IVOL    I       Input disk unit
C      TYPIN   C*2     Physical type of file
C   Outputs:
C      IND     I       FTAB pointer
C      CNO     I       Catalog slot containing map
C      CATBLK  I(256)  Buffer containing current catalog block
C      WBUFF   I(256)  Working buffer for CATIO and CATDIR
C      IERR    I       Error output: 0 = OK
C                         2 = Can't open WRIT because file busy
C                             or can't READ because file marked WRITE
C                         3 = File not found
C                         4 = Catalog i/o error
C                         5 = Illegal OP code
C                         6 = Can't open file
C-----------------------------------------------------------------------
      CHARACTER OP*4, NAMEIN*(*), CLASIN*(*), TYPIN*2
      INTEGER   IVOL, SEQIN, USID, LUN, IND, CNO, CATBLK(256),
     *   WBUFF(256), IERR
C
      CHARACTER  STAT*4, UNCODE(4)*4, STCODE(4)*4, MNAME*48,
     *   OPCODE(4)*4
      INTEGER   IOP, NOP, OPERR, IER, JER, DUM
      LOGICAL   T, EXCL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA DUM, OPERR /1, 5/
      DATA T /.TRUE./
      DATA STCODE /'READ', 'WRIT', 'REST', 'WRIT'/
      DATA UNCODE /'CLRD', 'CLWR', 'REST', 'CLWR'/
      DATA NOP, OPCODE /4, 'READ', 'WRIT', 'INIT', 'HDWR'/
C-----------------------------------------------------------------------
C                                       get op code
      IERR = 0
      JER = 4
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 20
 10      CONTINUE
      IERR = OPERR
      GO TO 999
C                                       Find and open map
 20   CNO = 1
      CALL CATDIR ('SRCH', IVOL, CNO, NAMEIN, CLASIN, SEQIN, TYPIN,
     *    USID, STAT, WBUFF, IER)
      IF (IER.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) NAMEIN, CLASIN, SEQIN, TYPIN, USID
         CALL MSGWRT (7)
         IERR = 3
         GO TO 999
C                                       read catblock: set end status
 30   CALL CATIO ('READ', IVOL, CNO, CATBLK, STCODE(IOP), WBUFF, JER)
      IF (JER.EQ.0) GO TO 50
C                                       Catalog i/o errors
         IF (JER.GT.3) GO TO 40
            IERR = 4
            WRITE (MSGTXT,1030) JER
            GO TO 980
C                                       Branch according to OP
 40      IF (IOP.EQ.3) GO TO 50
C                                       For READ now an error
C                                       For write on busy real error
         IERR = 2
         WRITE (MSGTXT,1040) NAMEIN, CLASIN, SEQIN
         GO TO 980
C                                       Open the file
 50   CALL ZPHFIL (TYPIN, IVOL, CNO, 1, MNAME, IER)
      EXCL = (IOP.EQ.2) .OR. (IOP.EQ.3)
      CALL ZOPEN (LUN, IND, IVOL, MNAME, T, EXCL, T, IER)
      IF (IER.EQ.0) GO TO 999
         IERR = 6
         WRITE (MSGTXT,1050) NAMEIN, CLASIN, SEQIN, IER
         GO TO 980
C-----------------------------------------------------------------------
C                                       errors
 980  CALL MSGWRT (8)
      IF (JER.NE.4) CALL CATDIR ('CSTA', IVOL, CNO, NAMEIN, CLASIN, DUM,
     *   TYPIN, DUM, UNCODE(IOP), CATBLK, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A12,'.',A6,'.',I4,' ',A2,I5,' NOT FOUND')
 1030 FORMAT ('MAPOPN: CATALOG I/O ERROR =',I5)
 1040 FORMAT ('MAPOPN: ERROR ',A12,'.',A6,'.',I4,' TOO BUSY')
 1050 FORMAT ('CANT OPEN ',A12,'.',A6,'.',I4,' IER=',I4)
      END
