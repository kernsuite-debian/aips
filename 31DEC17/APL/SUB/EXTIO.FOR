      SUBROUTINE EXTIO (OPCODE, LUN, IND, IRNO, RECORD, BUFFER, IERR)
C-----------------------------------------------------------------------
C! does random access IO to extension files of the EXTINI/EXTIO type
C# EXT-util
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
C   EXTIO does random access I/O to an extension files.  Mixed reads
C   and writes are allowed if EXTINI was called 'WRIT'
C   Inputs:
C      OPCODE         C*4  Opcode 'READ','WRIT','CLOS'
C      LUN            I    Logical unit number
C      IND            I    FTAB pointer
C      IRNO           I    Logical record no. 0=> next.
C      RECORD( )      I    Array containing record to be written
C      BUFFER( )      I    Work buffer = 512 bytes + enough 512 byte
C                          blocks for at least one full logical record.
C   Output:
C      RECORD( )      I    Array containing record read.
C      BUFFER( )      I    buffer.
C      IERR           I    Return error code 0 => OK
C                                1 => file not open
C                                2 => input error
C                                3 => I/O error
C                                4 => attempt to read past end of data
C                                     or write past log. or phys.
C                                     record 32766.
C   IMPORTANT NOTE:  the contents of BUFFER should not be changed
C   except by EXTIO between the time EXTINI is called until the file
C   is closed.  The exception is that the user portion of the header
C   record is available.
C   Programmer: W. D. Cotton, Sept. 1981
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   LUN, IND, IRNO, RECORD(*), BUFFER(*), IERR
C
      INTEGER   LRNO, I, IER, INDEX, IOP, LIMIT, LROFF, NADD, NEXP,
     *   NPB, JBUF
      CHARACTER PNAME*48
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Determine logical rec. no.
      IERR = 2
      LRNO = IRNO
      IF (LRNO.LE.0) LRNO = BUFFER(56) + 1
C                                       Determine opcode.
      IOP = 0
      IF (OPCODE.EQ.'READ') IOP = 1
      IF (OPCODE.EQ.'WRIT') IOP = 2
      IF (OPCODE.EQ.'CLOS') GO TO 200
C                                       Unknown opcode.
      IF (IOP.GT.0) GO TO 20
         WRITE (MSGTXT,1000) OPCODE
         GO TO 990
C                                       Check record no.
 20   IERR = 4
      IF ((IOP.EQ.1) .AND. (LRNO.GT.BUFFER(4))) GO TO 999
C                                       Determine phys. block
      NPB = 1
      IF (BUFFER(7).LT.0) NPB = (1 - LRNO) * BUFFER(7) + 1
      IF (BUFFER(7).GT.0) NPB = (LRNO-1)/BUFFER(7) + 1
C                                       Determine off of log.
      LROFF = 257
      IF (BUFFER(7).GT.0) LROFF = (LRNO - (NPB-1)*BUFFER(7) - 1) *
     *   BUFFER(53) + 257
C                                       New record required
      IF (BUFFER(55).EQ.NPB) GO TO 55
C                                       If write-enable check if going
C                                       to new phys. rec - if so write
         LIMIT = -BUFFER(7)
         LIMIT = MAX (1, LIMIT)
C                                       Write OLD block
C                                       Don't write on first access
         IF (BUFFER(54).NE.2) GO TO 35
            IF (BUFFER(55).LE.0) GO TO 35
            JBUF = BUFFER(55)+1
            DO 30 I = 1,LIMIT
               INDEX = I * 256 + 1
               CALL ZFIO ('WRIT', LUN, IND, JBUF, BUFFER(INDEX), IER)
               JBUF = JBUF + 1
               IF (IER.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) 'WRITE', IER
                  GO TO 990
 30            CONTINUE
C                                       Extend if necessary
 35      IF ((NPB+LIMIT.LE.BUFFER(1)) .AND. (LRNO.LE.BUFFER(3)))
     *      GO TO 45
            NADD = 0.1 * BUFFER(3) + 1.5
            NADD = MIN (BUFFER(2), NADD)
            IF (BUFFER(7).LE.0) NEXP = NADD * ABS(BUFFER(7))
            IF (BUFFER(7).GT.0) NEXP = NADD / BUFFER(7) + 1
            IERR = 3
            CALL H2CHR (48, 1, BUFFER(17), PNAME)
            CALL ZEXPND (LUN, BUFFER(29), PNAME, NEXP, IER)
            IF (IER.EQ.0) GO TO 40
               WRITE (MSGTXT,1035) IER
               GO TO 990
C                                       Update info
 40         BUFFER(1) = BUFFER(1) + NEXP
            NADD = NEXP * BUFFER(7)
            IF (BUFFER(7).LT.0) NADD = NEXP / ABS (BUFFER(7))
            BUFFER(3) = BUFFER(3) + NADD
C                                       New record read.
 45      IF ((LRNO.GT.BUFFER(4)) .AND. (BUFFER(55).GT.0)) GO TO 55
            JBUF = NPB + 1
            DO 50 I = 1,LIMIT
               INDEX = 256 * I + 1
               CALL ZFIO ('READ', LUN, IND, JBUF, BUFFER(INDEX), IER)
               JBUF = JBUF + 1
               IF (IER.EQ.0) GO TO 50
                  WRITE (MSGTXT,1020) 'READ', IER
                  GO TO 990
 50            CONTINUE
 55      BUFFER(55) = NPB
C                                       Do data transfer
      BUFFER(56) = LRNO
      IERR = 0
      LIMIT = BUFFER(53)
      IF (IOP.EQ.1) CALL COPY (LIMIT, BUFFER(LROFF), RECORD)
      IF (IOP.EQ.1) GO TO 999
         CALL COPY (LIMIT, RECORD, BUFFER(LROFF))
         BUFFER(4) = MAX (BUFFER(4), LRNO)
         GO TO 999
C                                       Close file
C                                       If write-enabled write.
 200  IF (BUFFER(54).EQ.1) GO TO 220
C                                       Write header
         CALL ZFIO ('WRIT', LUN, IND, 1, BUFFER, IER)
         IF (IER.EQ.0) GO TO 205
            IERR = 3
            WRITE (MSGTXT,1020) 'WRITE', IER
            GO TO 990
C                                       Write data
 205     IF (BUFFER(55).LE.0) GO TO 220
            LIMIT = - BUFFER(7)
            LIMIT = MAX (1, LIMIT)
            JBUF = BUFFER(55) + 1
            DO 210 I = 1,LIMIT
               INDEX = 256 * I + 1
               CALL ZFIO ('WRIT', LUN, IND, JBUF, BUFFER(INDEX), IER)
               JBUF = JBUF + 1
               IF (IER.EQ.0) GO TO 210
                  IERR = 3
                  WRITE (MSGTXT,1020) 'WRITE', IER
                  GO TO 990
 210           CONTINUE
C                                       Close.
 220  CALL ZCLOSE (LUN, IND, IER)
      IERR = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTIO: UNKNOWN OPCODE: ',A4)
 1020 FORMAT ('EXTIO: ',A,' ERROR ',I3)
 1035 FORMAT ('EXTIO: ZEXPND ERROR',I3,' EXPANDING FILE')
      END
