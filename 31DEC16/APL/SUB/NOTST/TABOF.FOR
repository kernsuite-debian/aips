      SUBROUTINE TABOF (OPCODE, OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME,
     *   SOURCE, IANT, SUBID, FREQID, ANFLAG, STATUS, IERR)
C----------------------------------------------------------------------
C! I/O to Online Flags table (following init. by OFINI)
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2003, 2008, 2013
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
C----------------------------------------------------------------------
C   Does I/O to a Online Flags table.  Used after setup by OFINI.
C   Inputs:
C      OPCODE       C*4         Operation code:
C                               'READ' = read entry from table
C                               'WRIT' = write entry in table
C                               'CLOS' = close file, flush on write
C      OFBUFF       I(*)        I/O buffer and related storage, also
C                               defines file if open. Should have
C                               been returned by OFINI.
C   Input/Output:
C      IOFRNO       I           Next row to read or write
C      OFKOLS       I(MAXOFC)   The column pointer array in order:
C                               TIME, DTIME, SRCID, IANT, SUBID,
C                               FREQID, FLAG
C      OFNUMV       I(MAXOFC)   Element count in each column.
C      TIME         R           Time (days wrt reference day)
C      SOURCE       I           source ID
C      IANT         I           antenna number
C      SUBID        I           subarray ID
C      FREQID       I           frequency ID
C      ANFLAG       I           Antenna-based flag bit pattern
C      STATUS       I(2,*)      Status level (pol, IF)
C   Output:
C      IERR         I           Return code (0=>ok; else error)
C                               Note: -1 => read by not selected
C----------------------------------------------------------------------
      INCLUDE 'INCS:DOFV.INC'
      CHARACTER OPCODE*4
      INTEGER   OFBUFF(512), IOFRNO, OFKOLS(MAXOFC), OFNUMV(MAXOFC),
     *   SOURCE, IANT, SUBID, FREQID, ANFLAG, STATUS(2,*), IERR
      REAL      TIME
C
      REAL      RECR(MXSPOF)
      INTEGER   RECI(MXSPOF), I, J
      EQUIVALENCE (RECR, RECI)
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, IOFRNO, RECI, OFBUFF, IERR)
         IF (IERR.GT.0) CALL TABERR ('CLOS', 'TABIO', 'TABOF', IERR)
C                                       Do I/O
      ELSE
C                                       If write fill RECORD
         IF (OPCODE.NE.'READ') THEN
C                                       Time
            RECR(OFKOLS(OFRTIM)) = TIME
C                                       source ID
            RECI(OFKOLS(OFISID)) = SOURCE
C                                       antenna number
            RECI(OFKOLS(OFIANT)) = IANT
C                                       subarray ID
            RECI(OFKOLS(OFISUB)) = SUBID
C                                       frequency ID
            RECI(OFKOLS(OFIFQI)) = FREQID
C                                       flag values
            RECI(OFKOLS(OFIANF)) = ANFLAG
C                                       pol 1
            J = OFKOLS(OFIST1) - 1
            DO 10 I = 1,OFNUMV(OFIST1)
               J = J + 1
               RECI(J) = STATUS(1,I)
 10            CONTINUE
C                                       pol 2
            DO 20 I = 1,OFNUMV(OFIST2)
               J = J + 1
               RECI(J) = STATUS(2,I)
 20            CONTINUE
            END IF
C                                       Process record
 100     CALL TABIO (OPCODE, 0, IOFRNO, RECR, OFBUFF, IERR)
         IOFRNO = IOFRNO + 1
         IF (IERR.GT.0) THEN
            CALL TABERR (OPCODE, 'TABIO', 'TABOF', IERR)
            GO TO 999
C                                       If READ pick data from record
         ELSE IF (OPCODE.EQ.'READ') THEN
C                                       Time
            TIME = RECR(OFKOLS(OFRTIM))
C                                       source ID
            SOURCE = RECI(OFKOLS(OFISID))
C                                       antenna number
            IANT = RECI(OFKOLS(OFIANT))
C                                       subarray ID
            SUBID = RECI(OFKOLS(OFISUB))
C                                       frequency ID
            FREQID = RECI(OFKOLS(OFIFQI))
C                                       flag values
            ANFLAG = RECI(OFKOLS(OFIANF))
C                                       pol 1
            J = OFKOLS(OFIST1) - 1
            DO 110 I = 1,OFNUMV(OFIST1)
               J = J + 1
               STATUS(1,I) = RECI(J)
 110           CONTINUE
C                                       pol 2
            DO 120 I = 1,OFNUMV(OFIST2)
               J = J + 1
               STATUS(2,I) = RECI(J)
 120           CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
